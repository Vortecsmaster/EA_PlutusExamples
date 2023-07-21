{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module DarjanCoins where

import           PlutusTx                        (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed)
<<<<<<< Updated upstream
import           PlutusTx.Builtins.Class         (stringToBuiltinByteString)
import           PlutusTx.Prelude                (Bool (..),traceIfFalse, otherwise, Integer, ($), (<=), (&&), any,map)
=======
import           PlutusTx.Builtins
import           PlutusTx.Builtins.Class         (toBuiltin)
import           PlutusTx.Prelude                (Bool (..),traceIfFalse, otherwise, Integer, ($), (<=), (&&), any, foldr)
>>>>>>> Stashed changes
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
        p1pkh = PubKeyHash { getPubKeyHash = toBuiltin (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [56,102,100,50,97,102,51,49,56,102,101,54,102,100,55,97,56,98,50,102,53,54,56,54,49,98,55,100,100,97,51,49,50,52,49,49,50,56,49,54,49,54,98,57,48,50,57,53,51,97,98,102,55,49,50,49])}

        info :: TxInfo
        info = scriptContextTxInfo sContext

<<<<<<< Updated upstream
        -- publicKeyHash = PubKeyHash $ toBuiltin $ bytesFromHex "27396e442d87d62131ba6406a5d6fe0eda159eb18904dfa6eeb026de"

        conditions :: [PubKeyHash]
        conditions = [(PubKeyHash $ toBuiltin $ bytesFromHex "8fd2af318fe6fd7a8b2f56861b7dda312411281616b902953abf7121")]
            -- (PubKeyHash (toBuiltin ::ByteString)), -- PKH alice
            -- (PubKeyHash (toBuiltin x::ByteString))  -- PKH bob
            --map func list
=======
        -- conditions :: [PubKeyHash]
        -- conditions = [
        --     (PubKeyHash "c4034310db8742a0c48539c26aa9890d10961925ffcde9de450581cc"), -- PKH alice
        --     (PubKeyHash "d70540296a8155451197472f28ab6c5c4e9830d5c2f1b615307ed10b")  -- PKH bob
        --     ]
>>>>>>> Stashed changes

        checkSignaturePassed :: Bool
        checkSignaturePassed = txSignedBy info $ p1pkh

{-# INLINABLE wrappedDarjanCoinPolicy #-}
wrappedDarjanCoinPolicy :: BuiltinData -> BuiltinData -> ()
wrappedDarjanCoinPolicy = wrapPolicy $ darjanCoins

darjanCoinPolicy :: MintingPolicy
darjanCoinPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrappedDarjanCoinPolicy ||])

-- Serialised Scripts and Values 

saveDarjanCoinPolicy :: IO ()
<<<<<<< Updated upstream
saveDarjanCoinPolicy = writePolicyToFile "./testnet/handson07.plutus" darjanCoinPolicy
=======
saveDarjanCoinPolicy = writePolicyToFile "./testnet/1signMP.plutus" darjanCoinPolicy
>>>>>>> Stashed changes

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveAll :: IO ()
saveAll = do
            saveDarjanCoinPolicy
            saveUnit