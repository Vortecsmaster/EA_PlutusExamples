{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module EAcoins where

import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (True))
import           Plutus.V2.Ledger.Api (BuiltinData, CurrencySymbol,
                                       MintingPolicy, ScriptContext,
                                       mkMintingPolicyScript)

import           Prelude              (IO)
import           Mappers          (wrapPolicy)
import           Serialization    (currencySymbol, writePolicyToFile) 

-- {-# INLINABLE eaCoins #-}
-- eaCoins :: Action -> ScriptContext -> Bool
-- eaCoins action sContext = True

-- {-# INLINABLE mkWrappedFreePolicy #-}
-- mkWrappedEAcoinsPolicy :: BuiltinData -> BuiltinData -> ()
-- mkWrappedEAcoinsPolicy = wrapPolicy eaCoins

-- eaCoinsPolicy :: MintingPolicy
-- eaCoinsPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkWrappedEAcoinsPolicy ||])

-- --SERIALIZATION INTO 

-- saveFreePolicy :: IO ()
-- saveFreePolicy = writePolicyToFile "testnet/EAcoins.plutus" freePolicy