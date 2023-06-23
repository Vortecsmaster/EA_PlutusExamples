{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module EAcoins where

import           PlutusTx               (Builtindata, unstableMakeIsData, makeIsDataIndexed)
import           PlutusTx.Prelude       (Bool (..))
import           Plutus.V2.Ledger.Api   (BuiltinData, CurrencySymbol,
                                         MintingPolicy, ScriptContext,
                                         mkMintingPolicyScript)
--Serialization
import           Mappers                (wrapPolicy)
import           Serialization          (currencySymbol, writePolicyToFile) 
import           Prelude                (IO)

-- ON-CHAIN CODE

data Action = Owner | Time | Price
unstableMakeIsData ''Action

{-# INLINABLE eaCoins #-}
eaCoins :: Action -> ScriptContext -> Bool
eaCoins action sContext = case action of
                            Owner   -> traceIfFalse    "Not signed properly!"  signedByOwner
                            Time    -> traceIfFalse    "Your run out of time!" timeLimitNotReached                                         
                            Price   -> traceIfFalse    "Price is not covered"  priceIsCovered
    where
        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ owner datum

        timeLimitNotReached :: Bool
        timeLimitNotReached = contains (to $ POSIXTime 1704067199000) $ txInfoValidRange info 

        priceIsCovered :: Bool
        priceIsCovered =  assetClassValueOf (valueSpent info)  (AssetClass (adaSymbol,adaToken)) > 100000000

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