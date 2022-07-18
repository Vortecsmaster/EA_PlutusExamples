{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}

module MathBountyDeadline where

import           Control.Monad             (void)
--import qualified Data.ByteString.Char8     as C
import Data.Default               (Default (..))
import Data.Text                  (Text)
import Data.Void
import Data.Map as Map
--import           Codec.Serialise
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))
import           Plutus.Contract
import           Plutus.Trace.Emulator  as Emulator
import           Ledger                    
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Playground.Contract
import qualified Prelude
import Prelude (IO, Show, String, show, Semigroup (..))
import           Text.Printf          (printf)
import Ledger.TimeSlot
import qualified Plutus.Trace as Trace
import Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras
--import qualified Data.OpenApi.Schema as OApi
import PlutusTx.IsData.Class (toBuiltinData)

--ON-CHAIN

data MathBountyDatum = MathBountyD
    { mBounty :: Integer
    , deadline :: POSIXTime
    } 
PlutusTx.unstableMakeIsData ''MathBountyDatum

{-# INLINABLE mathBountyValidator  #-}
mathBountyValidator :: MathBountyDatum -> Integer -> ScriptContext -> Bool
mathBountyValidator datum x sContext = traceIfFalse "Wrong guess!" ((mBounty datum) == x*x) &&
                                       traceIfFalse "Deadline passed!" deadlineReached
                                            
        where
            info :: TxInfo
            info = scriptContextTxInfo sContext

            deadlineReached :: Bool
            deadlineReached = contains (to $ deadline datum) $ txInfoValidRange info

data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

bountyValidator :: Scripts.TypedValidator MathBounty
bountyValidator = Scripts.mkTypedValidator @MathBounty
    $$(PlutusTx.compile [|| mathBountyValidator ||])
    $$(PlutusTx.compile [|| wrapping ||])
     where
       wrapping  = Scripts.wrapValidator @MathBountyDatum @Integer

validator :: Validator
validator = Scripts.validatorScript bountyValidator

bountyScript :: Ledger.ValidatorHash  --Script  
bountyScript = Scripts.validatorHash bountyValidator --unValiatorScript validator

bountyAddress :: Ledger.Address       --Address
bountyAddress = scriptAddress validator  --Ledger.scriptAddress validator

--OFF-CHAIN
data BountyParams = BP 
                  { bMathBounty :: Integer
                  , bAmount     :: Integer
                  , bDeadline   :: POSIXTime
                  } deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

type MathBountySchema = 
        Endpoint "bounty" BountyParams
    .\/ Endpoint "solution" Integer

bounty :: BountyParams -> Contract () MathBountySchema Text ()
bounty (BP bounty amount deadline) = do
    let datum = MathBountyD bounty deadline
        tx = Constraints.mustPayToTheScript datum (Ada.lovelaceValueOf amount)
    ledgerTx <- submitTxConstraints bountyValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Some bounty created of amount =  %s" (show amount)

--data SolutionParams = SP { guess :: Integer }

solution ::  Integer -> Contract () MathBountySchema Text ()
solution guess = do
                 now <- currentTime
                 unspentOutput <- utxosAt bountyAddress
                 logInfo @String $ printf "The utxos: %s " (show $ Map.toList unspentOutput )
                 case Map.toList unspentOutput of
                  []             -> logInfo @String $ printf "No UTxOs on the Contract!"
                  (oref,a):utxos -> do
                                    let lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) <>
                                                  Constraints.otherScript validator 
                                    let tx = Constraints.mustSpendScriptOutput oref (Redeemer $ toBuiltinData guess) <> Constraints.mustValidateIn (to now)
                                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                    logInfo @String $ printf "Proposed solution is: %s" (show guess)                    
 
endpoints :: Contract () MathBountySchema Text ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where
    bounty' =  endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

mkSchemaDefinitions ''MathBountySchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints
    callEndpoint @"bounty" h1 $ BP
                    { bMathBounty = 100
                  , bAmount     = 51000000
                  , bDeadline   = 1596059101000
                    }
    void $ Emulator.waitNSlots 5
    callEndpoint @"solution" h2 $ 5
    void $ Emulator.waitNSlots 2
    callEndpoint @"solution" h3 $ 10
    void $ Emulator.waitNSlots 11