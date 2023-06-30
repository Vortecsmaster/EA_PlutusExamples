{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module EAnft where

import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (True), (&&))
import           Plutus.V2.Ledger.Api (BuiltinData, CurrencySymbol,
                                       MintingPolicy, ScriptContext,
                                       mkMintingPolicyScript)

import           Prelude              (IO)
import           Mappers          (wrapPolicy)
import           Serialization    (currencySymbol, writePolicyToFile) 

{-# INLINABLE eaNFT #-}
eaNFT :: TxOutRef -> Bool -> ScriptContext -> Bool
eaNFT utxo category sContext = approvedMinting 
    where
        
{-# INLINABLE mkWrappedFreePolicy #-}
wrappedEAnftPolicy :: BuiltinData -> BuiltinData -> ()
wrappedEAnftPolicy = wrapPolicy eaNFT

eaNFTPolicy :: MintingPolicy
eaNFTPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkWrappedEAcoinsPolicy ||]) `apply` 

--THE ON-CHAIN CODE
data ConditionsDatum = Conditions { owner :: PubKeyHash
                                  , timelimit :: POSIXTime
                                  , price :: Integer
                                  }
unstableMakeIsData ''ConditionsDatum

data ActionsRedeemer = Owner | Time | Price
unstableMakeIsData '' ActionsRedeemer


{-# INLINABLE conditionator #-}
conditionator :: ConditionsDatum -> ActionsRedeemer -> ScriptContext -> Bool
conditionator datum redeemer sContext = case redeemer of
                                         Owner   -> traceIfFalse    "Not signed properly!"  signedByOwner
                                         Time    -> traceIfFalse    "Your run out of time!" timeLimitNotReached                                         
                                         Price   -> traceIfFalse    "Price is not covered"  priceIsCovered
    where
        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ owner datum

        timeLimitNotReached :: Bool
        timeLimitNotReached = contains (to $ timelimit datum) $ txInfoValidRange info 

        priceIsCovered :: Bool
        priceIsCovered =  assetClassValueOf (valueSpent info)  (AssetClass (adaSymbol,adaToken)) > price datum

        info :: TxInfo
        info = scriptContextTxInfo sContext


mappedCommonConditions :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mappedCommonConditions = wrapValidator conditionator

conditionsValidator :: Validator
conditionsValidator =  PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedCommonConditions ||])

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

saveConditionsValidator :: IO ()
saveConditionsValidator =  writeValidatorToFile "./testnet/conditionator.plutus" conditionsValidator

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveDatum :: IO ()
saveDatum  = writeDataToFile "./testnet/datum.json" (Conditions "" 1686837045000 50)

saveRedeemerOwner :: IO ()
saveRedeemerOwner = writeDataToFile "./testnet/redeemOwner.json" Owner

saveRedeemerTime :: IO ()
saveRedeemerTime = writeDataToFile "./testnet/redeemTime.json" Time

saveRedeemerPrice :: IO ()
saveRedeemerPrice = writeDataToFile "./testnet/redeemPrice.json" Price