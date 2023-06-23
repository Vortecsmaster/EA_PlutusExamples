{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings  #-}

module LearnersCoins where

import           PlutusTx                        (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed)
import           PlutusTx.Prelude                (Bool (..),traceIfFalse, otherwise, Integer, ($), (<=), (&&))
import           Plutus.V2.Ledger.Api            (BuiltinData, CurrencySymbol,
                                                 MintingPolicy, ScriptContext,
                                                 mkMintingPolicyScript)
import           Plutus.V1.Ledger.Value       as PlutusV1
import           Plutus.V1.Ledger.Interval      (contains, to) 
import           Plutus.V2.Ledger.Api        as PlutusV2
import           Plutus.V2.Ledger.Contexts      (txSignedBy, valueSpent, ownCurrencySymbol)

--Serialization
import           Mappers                (wrapPolicy)
import           Serialization          (currencySymbol, writePolicyToFile,  writeDataToFile) 
import           Prelude                (IO)

-- ON-CHAIN CODE


{-# INLINABLE learnersCoins #-}
learnersCoins :: POSIXTime -> ScriptContext -> Bool
learnersCoins deadline sContext = traceIfFalse ("You can no longer mint, timelimit reached!") onTime &&
                                  traceIfFalse ("You can not mint more than 100 tokens per tx!") noMoreThan100
    where
        info :: TxInfo
        info = scriptContextTxInfo sContext

        onTime :: Bool
        onTime =  contains (to $ deadline) $ txInfoValidRange info

        noMoreThan100 :: Bool
        noMoreThan100 = assetClassValueOf (txInfoMint info) (AssetClass ((ownCurrencySymbol sContext), (TokenName "4541636f696e73"))) <= 100

        

{-# INLINABLE wrappedLearnersCoinsPolicy #-}
wrappedLearnersCoinsPolicy :: BuiltinData -> BuiltinData -> ()
wrappedLearnersCoinsPolicy = wrapPolicy learnersCoins

learnersCoinsPolicy :: MintingPolicy
learnersCoinsPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrappedLearnersCoinsPolicy ||])

-- Serialised Scripts and Values 

saveLearnersCoinsPolicy :: IO ()
saveLearnersCoinsPolicy = writePolicyToFile "testnet/LearnersCoins.plutus" learnersCoinsPolicy

saveDeadline :: IO ()
saveDeadline = writeDataToFile "./testnet/deadline.json" (POSIXTime 1687363057000)

saveAll :: IO ()
saveAll = do
            saveLearnersCoinsPolicy
            saveDeadline