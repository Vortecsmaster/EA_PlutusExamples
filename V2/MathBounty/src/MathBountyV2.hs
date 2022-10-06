{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module MathBountyV2 where

--PlutusTx 
import           PlutusTx                             (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins                    as Builtins
import           PlutusTx.Prelude                     hiding (Semigroup(..), unless)
--Contract Monad
import           Plutus.Contract               
--Ledger 
import           Ledger                               hiding (singleton, ScriptContext)
import qualified Ledger.Address                       as V1Address
import           Ledger.Constraints                   as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Scripts               as Scripts               
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions
import           Plutus.V2.Ledger.Contexts            as Contexts
import qualified Plutus.V2.Ledger.Api                 as PlutusV2            
import           Ledger.Ada                           as Ada 
--Trace Emulator
import           Plutus.Trace
import qualified Plutus.Trace.Emulator                as Emulator
import qualified Wallet.Emulator.Wallet               as Wallet
{-Plutus Playground (broken)
import           Playground.Contract            (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH                  (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types               (KnownCurrency (..))
--"Normal" Haskell -}
import           Control.Monad                        hiding (fmap)
import           GHC.Generics                         (Generic)
import           Data.Aeson                           (ToJSON, FromJSON)
import           Data.Map                             as Map
import           Data.Text                            (Text)
import           Data.Void                            (Void)
import           Prelude                              (IO, Semigroup (..), String, Show (..))
import           Text.Printf                          (printf)
import           Control.Monad.Freer.Extras           as Extras


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

data MathBountyDatum = MBD 
                     { mbBounty    :: Integer
                     , mbDeadline :: POSIXTime }  

PlutusTx.makeIsDataIndexed ''MathBountyDatum [('MBD,0)]        -- At compile time write an instance of this data type (MyWonderFullRedeemer) on the IsData typeclass

{-# INLINABLE mathBountyValidator  #-}
mathBountyValidator :: MathBountyDatum -> Integer -> PlutusV2.ScriptContext -> Bool
mathBountyValidator datum x sContext = traceIfFalse "Wrong guess!" ((mbBounty datum) == x*x) &&
                                       traceIfFalse "Deadline passed!" deadlineReached
       where
            info :: Contexts.TxInfo
            info = Contexts.scriptContextTxInfo sContext

            deadlineReached :: Bool
            deadlineReached = contains (to $ mbDeadline datum) $ PlutusV2.txInfoValidRange info
                           -- (to $ mbDeadline datum) `contains` (txInfoValidRange info)

data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

bountyValidator :: Scripts.TypedValidator MathBounty
bountyValidator = Scripts.mkTypedValidator @MathBounty
    $$(PlutusTx.compile [|| mathBountyValidator ||])
    $$(PlutusTx.compile [|| wrapping ||])
     where
       wrapping  = Scripts.mkUntypedValidator @MathBountyDatum @Integer

validator :: Validator
validator = Scripts.validatorScript bountyValidator                   

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash bountyValidator                      

bountyAddress :: Ledger.Address
bountyAddress = Scripts.validatorAddress bountyValidator                 -- New function to derive the address, included in the Utils library
--bountyAddress = scriptAddress valiator 

--THE OFFCHAIN CODE
data BountyParams = BP
                  { bpBounty  :: Integer
                  , bpAmount :: Integer
                  , bpDeadline :: POSIXTime
                  } deriving (Generic,ToJSON,FromJSON,Show)

type MathBountySchema =
        Endpoint "bounty" BountyParams
    .\/ Endpoint "solution" Integer  

bounty :: BountyParams -> Contract () MathBountySchema Text ()
bounty (BP bounty amount deadline) = do 
                                  let datum = MBD 
                                              { mbBounty    = bounty
                                              , mbDeadline = deadline}  
                                      tx = Constraints.mustPayToTheScript datum (Ada.lovelaceValueOf amount)                                                                     
                                  ledgerTx <- submitTxConstraints bountyValidator tx
                                  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                  Plutus.Contract.logInfo @String $ printf "Bounty created of amount = %s" (show amount)


solution :: Integer -> Contract () MathBountySchema Text ()
solution guess = do
                 now <- currentTime
                 utxos <- utxosAt bountyAddress
                 Plutus.Contract.logInfo @String $ printf "The utxos: %s " (show $ Map.toList utxos)
                 case Map.toList utxos of
                    []                 -> Plutus.Contract.logInfo @String $ printf "No UTxOs on the Contract!"
                    (oref,a):xs        -> do
                                          let lookups = Constraints.plutusV2OtherScript validator   <>
                                                        Constraints.unspentOutputs (Map.fromList [(oref,a)])
                                              tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusV2.toBuiltinData guess) <>
                                                        Constraints.mustValidateIn (to now) -- include all posible UTxOs (mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs] )
                                          ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                          Plutus.Contract.logInfo @String $ printf "Proposed solution was: %s " (show guess)

endpoints :: Contract () MathBountySchema Text ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where 
    bounty'   = endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

-- Playground broken at the moment - September 2022
-- mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for the playground
-- mkKnownCurrencies []                                                                                 -- MakeKnown currencies for the playground to have some ADA available


--SIMULATION

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet.knownWallet 1) endpoints
    h2 <- activateContractWallet (Wallet.knownWallet 2) endpoints
    h3 <- activateContractWallet (Wallet.knownWallet 3) endpoints
    callEndpoint @"bounty" h1 $ BP
                              { bpBounty  = 100
                              , bpAmount = 51000000
                              , bpDeadline = POSIXTime 1596059211000
                              }
    void $ Emulator.waitNSlots 11
    callEndpoint @"solution" h2 5
    void $ Emulator.waitNSlots 11
    callEndpoint @"solution" h3 10
    s <- Emulator.waitNSlots 11
    Extras.logInfo $ "End of Simulation at slot " ++ show s