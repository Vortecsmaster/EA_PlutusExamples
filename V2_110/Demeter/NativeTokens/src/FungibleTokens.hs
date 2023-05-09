{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module FungibleTokens where

import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (True))
import           Plutus.V2.Ledger.Api (BuiltinData, CurrencySymbol,
                                       MintingPolicy, ScriptContext,
                                       mkMintingPolicyScript)

import           Prelude              (IO)
import           Utils            (currencySymbol, wrapPolicy,
                                       writePolicyToFile)

{-# INLINABLE asPolicy #-}
asPolicy :: () -> ScriptContext -> Bool
asPolicy () _ = True

{-# INLINABLE mkWrappedFreePolicy #-}
mkWrappedFreePolicy :: BuiltinData -> BuiltinData -> ()
mkWrappedFreePolicy = wrapPolicy asPolicy

freePolicy :: MintingPolicy
freePolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkWrappedFreePolicy ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveFreePolicy :: IO ()
saveFreePolicy = writePolicyToFile "testnet/as.policy" freePolicy

freeCurrencySymbol :: CurrencySymbol