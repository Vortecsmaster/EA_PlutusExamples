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

module MathBounty where

import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))

import           Ledger                    
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Ledger.TimeSlot
import           PlutusTx.IsData.Class (toBuiltinData)

import           Plutus.Contract
import           Playground.Contract

import qualified Plutus.Trace              as Trace
import           Plutus.Trace.Emulator     as Emulator
import           Wallet.Emulator.Wallet

import           Control.Monad             (void)
import           Data.Default              (Default (..))
import           Data.Text                 (Text)
import           Data.Void
import           Data.Map                  as Map
import           Prelude (IO, Show, String, show, Semigroup (..))
import           Text.Printf          (printf)
import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)

--ON-CHAIN CODE

data MathBountyDatum = MBD 
                      { mbdMath :: Integer
                      , mbdDeadline :: POSIXTime
                      }
                      
PlutusTx.makeIsDataIndexed ''MathBountyDatum [('MBD,0)]

{- TAKE OUT THIS COMMENTS ON PLAYGROUND
--option 1
--deriving (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
--PlutusTx.makeLift ''MathBountyDatum

--option 2 (no problems with newtypes)
--PlutusTx.unsafeMakeIsData ''MathBountyDatum
-}


{-# INLINABLE mathBountyValidator #-}
mathBountyValidator :: MathBountyDatum -> Integer -> ScriptContext -> Bool 
mathBountyValidator datum redeemer sContext = traceIfFalse "Wrong X!" (mbdMath datum == redeemer*redeemer) &&
                                              traceIfFalse "Deadline passed" deadlineReached
    where
      info :: TxInfo
      info = scriptContextTxInfo sContext

      deadlineReached :: Bool
      deadlineReached =  (to $ mbdDeadline datum) `contains` (txInfoValidRange info)

--{-# INLINABLE info #-} 
--      info :: TxInfo
--      info = scriptContextTxInfo sContext

-- CLI lowerbound paramter be like = --invalid-before 61967066 \

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

bountyAddress :: Ledger.Address       
bountyAddress = scriptAddress validator  

--OFF-CHAIN

endpoints :: Contract () MathBountySchema Text ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where
    bounty' =  endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

type MathBountySchema = 
        Endpoint "bounty" BountyParams
    .\/ Endpoint "solution" Integer

data BountyParams = BP 
                  { bMathBounty :: Integer
                  , bAmount     :: Integer
                  , bDeadline   :: POSIXTime
                  } deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

bounty :: BountyParams -> Contract () MathBountySchema Text ()
bounty (BP bounty amount deadline) = do
                                   let datum = MBD bounty deadline
                                       tx = Constraints.mustPayToTheScript datum (Ada.lovelaceValueOf amount)
                                   ledgerTx <- submitTxConstraints bountyValidator tx
                                   void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                   logInfo @String $ printf "Some bounty created of amount = %d" (amount)

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
                                    logInfo @String $ printf "Proposed solution is: %d" (guess)     


mkSchemaDefinitions ''MathBountySchema
mkKnownCurrencies []

-- t0 of the Playground = 1596059091000, and every second is 1000

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