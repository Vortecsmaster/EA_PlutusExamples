{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module DarjanCoins where

import           PlutusTx                        (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed)
import           PlutusTx.Builtins.Class         (stringToBuiltinByteString)
import           PlutusTx.Prelude                (Bool (..),traceIfFalse, otherwise, Integer, ($), (<=), (&&), any,map)
import           Plutus.V2.Ledger.Api            (BuiltinData, CurrencySymbol,
                                                 MintingPolicy, ScriptContext,
                                                 mkMintingPolicyScript, toBuiltin)
import           Plutus.V1.Ledger.Value       as PlutusV1
import           Plutus.V1.Ledger.Interval      (contains, to) 
import           Plutus.V2.Ledger.Api        as PlutusV2
import           Plutus.V2.Ledger.Contexts      (txSignedBy, valueSpent, ownCurrencySymbol)

--Serialization
import           Mappers                (wrapPolicy)
import           Serialization          (currencySymbol, writePolicyToFile,  writeDataToFile)
import           Conversions            (bytesFromHex) 
import           Prelude                (IO)

-- ON-CHAIN CODE

{-# INLINABLE darjanCoins #-}
darjanCoins :: () -> ScriptContext -> Bool
darjanCoins _ sContext = traceIfFalse "Incorrect Signatures!" (checkSignaturePassed)
    where
        info :: TxInfo
        info = scriptContextTxInfo sContext

        -- publicKeyHash = PubKeyHash $ toBuiltin $ bytesFromHex "27396e442d87d62131ba6406a5d6fe0eda159eb18904dfa6eeb026de"

        conditions :: [PubKeyHash]
        conditions = [(PubKeyHash $ toBuiltin $ bytesFromHex "8fd2af318fe6fd7a8b2f56861b7dda312411281616b902953abf7121")]
            -- (PubKeyHash (toBuiltin ::ByteString)), -- PKH alice
            -- (PubKeyHash (toBuiltin x::ByteString))  -- PKH bob
            --map func list

        checkSignaturePassed :: Bool
        checkSignaturePassed = any (\x -> txSignedBy info $ x) conditions

{-# INLINABLE wrappedDarjanCoinPolicy #-}
wrappedDarjanCoinPolicy :: BuiltinData -> BuiltinData -> ()
wrappedDarjanCoinPolicy = wrapPolicy $ darjanCoins

darjanCoinPolicy :: MintingPolicy
darjanCoinPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrappedDarjanCoinPolicy ||])

-- Serialised Scripts and Values 

saveDarjanCoinPolicy :: IO ()
saveDarjanCoinPolicy = writePolicyToFile "./testnet/handson07.plutus" darjanCoinPolicy

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveAll :: IO ()
saveAll = do
            saveDarjanCoinPolicy
            saveUnit