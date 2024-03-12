{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE OverloadedStrings   #-}  --Enable passing strings as other character formats, like bytestring.

module CommonConditions where

--PlutusTx 
import                  PlutusTx                       (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed)
import                  PlutusTx.Prelude               (traceIfFalse, otherwise, (==), Bool (..), Integer, ($), (>))
import                  Plutus.V1.Ledger.Value      as PlutusV1
import                  Plutus.V1.Ledger.Interval      (contains, to) 
import                  Plutus.V2.Ledger.Api        as PlutusV2
import                  Plutus.V2.Ledger.Contexts      (txSignedBy, valueSpent)
--Serialization
import                  Wrappers                        (wrapValidator)
import                  Serialization                  (writeValidatorToFile, writeDataToFile)
import                  Prelude                         (IO)

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
                                         Time    -> traceIfFalse    "You run out of time!" timeLimitNotReached                                         
                                         Price   -> traceIfFalse    "Price value is not reached"  priceIsReached
    where
        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ owner datum

        timeLimitNotReached :: Bool
        timeLimitNotReached = contains (to $ timelimit datum) $ txInfoValidRange info 

        priceIsReached :: Bool
        priceIsReached =  assetClassValueOf (valueSpent info)  (AssetClass (adaSymbol,adaToken)) > price datum

        info :: TxInfo
        info = scriptContextTxInfo sContext


wrappedCommonConditions :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedCommonConditions = wrapValidator conditionator

conditionsValidator :: Validator
conditionsValidator =  PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedCommonConditions ||])

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

{- Serialised Scripts and Values -}

saveConditionsValidator :: IO ()
saveConditionsValidator =  writeValidatorToFile "./testnet/conditionator.plutus" conditionsValidator

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveDatum :: IO ()
saveDatum  = writeDataToFile "./testnet/CCdatum.json" (Conditions "8fd2af318fe6fd7a8b2f56861b7dda312411281616b902953abf7121" 1686837045000 50)

saveRedeemerOwner :: IO ()
saveRedeemerOwner = writeDataToFile "./testnet/CCredeemOwner.json" Owner

saveRedeemerTime :: IO ()
saveRedeemerTime = writeDataToFile "./testnet/CCredeemTime.json" Time

saveRedeemerPrice :: IO ()
saveRedeemerPrice = writeDataToFile "./testnet/CCredeemPrice.json" Price

saveAll :: IO ()
saveAll = do
            saveConditionsValidator
            saveUnit
            saveDatum
            saveRedeemerOwner
            saveRedeemerPrice
            saveRedeemerTime
