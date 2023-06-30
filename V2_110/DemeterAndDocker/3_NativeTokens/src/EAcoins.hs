{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings  #-}

module EAcoins where

import           PlutusTx                        (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed)
import           PlutusTx.Prelude                (Bool (..),traceIfFalse, otherwise, Integer, ($), (<=), (&&), (>))
import           Plutus.V2.Ledger.Api            (CurrencySymbol, MintingPolicy, ScriptContext, mkMintingPolicyScript)
import           Plutus.V2.Ledger.Api            as PlutusV2
import           Plutus.V1.Ledger.Value          as PlutusV1
import           Plutus.V1.Ledger.Interval      (contains, to) 
import           Plutus.V2.Ledger.Contexts      (txSignedBy, valueSpent, ownCurrencySymbol)
--Serialization
import           Mappers                (wrapPolicy)
import           Serialization          (currencySymbol, writePolicyToFile,  writeDataToFile) 
import           Prelude                (IO)

-- ON-CHAIN CODE

data Action = Owner | Time | Price
unstableMakeIsData ''Action

data OurRedeemer = { action :: Action
                   , owner :: PubKeyHash
                   , timelimit :: POSIXTime
                   , price :: Integer }

unstableMakeIsData ''OurRedeemer

{-# INLINABLE eaCoins #-}
eaCoins :: OurRedeemer -> ScriptContext -> Bool
eaCoins redeemer sContext = case action redeemer of
                            Owner   -> traceIfFalse    "Not signed properly!"  signedByOwner
                            Time    -> traceIfFalse    "Your run out of time!" timeLimitNotReached                                         
                            Price   -> traceIfFalse    "Price is not covered"  priceIsCovered
    where
        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ owner redeemer

        timeLimitNotReached :: Bool
        timeLimitNotReached = contains (to $ timelimit redeemer) $ txInfoValidRange info 

        priceIsCovered :: Bool
        priceIsCovered =  assetClassValueOf (valueSpent info)  (AssetClass (adaSymbol,adaToken)) > price redeemer

        info :: TxInfo
        info = scriptContextTxInfo sContext



{-# INLINABLE wrappedEAcoinsPolicy #-}
wrappedEAcoinsPolicy :: BuiltinData -> BuiltinData -> ()
wrappedEAcoinsPolicy = wrapPolicy eaCoins

eaCoinsPolicy :: MintingPolicy
eaCoinsPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrappedEAcoinsPolicy ||])

-- Serialised Scripts and Values 

saveEAcoinsPolicy :: IO ()
saveEAcoinsPolicy = writePolicyToFile "testnet/EAcoins.plutus" eaCoinsPolicy

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveRedeemerOwner :: IO ()
saveRedeemerOwner = writeDataToFile "./testnet/redeemOwner.json" Owner

saveRedeemerTime :: IO ()
saveRedeemerTime = writeDataToFile "./testnet/redeemTime.json" Time

saveRedeemerPrice :: IO ()
saveRedeemerPrice = writeDataToFile "./testnet/redeemPrice.json" Price

saveAll :: IO ()
saveAll = do
            saveEAcoinsPolicy
            saveUnit
            saveRedeemerOwner
            saveRedeemerPrice
            saveRedeemerTime