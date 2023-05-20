{-# LANGUAGE NoImplicitPrelude #-}

module Mappers 
  ( wrapValidator
  , wrapPolicy
  , wrapStakeValidator
  ) where

import           PlutusTx
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import           PlutusTx.Prelude            (Bool, BuiltinData, check, ($),(.))
import           Plutus.V2.Ledger.Api        (CurrencySymbol (CurrencySymbol)
                                             , MintingPolicy
                                             , MintingPolicyHash (MintingPolicyHash)
                                             , POSIXTime
                                             , Validator
                                             , ValidatorHash (ValidatorHash)
                                             , ScriptContext
                                             , UnsafeFromData
                                             , unsafeFromBuiltinData)
import qualified Cardano.Api                 as Api
import           Cardano.Api.Shelley         (Address (..))
import qualified Cardano.Api.Shelley         as Api
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Serialization               (policyToScript, validatorToScript)

{-# INLINABLE wrapValidator #-}
wrapValidator :: ( UnsafeFromData a
                 , UnsafeFromData b
                 )
              => (a -> b -> ScriptContext -> Bool)
              -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapValidator f a b ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)

{-# INLINABLE wrapPolicy #-}
wrapPolicy :: UnsafeFromData a
           => (a -> ScriptContext -> Bool)
           -> (BuiltinData -> BuiltinData -> ())
wrapPolicy f a ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData ctx)

{-# INLINABLE wrapStakeValidator #-}
wrapStakeValidator :: UnsafeFromData a
                     => (a -> ScriptContext -> Bool)
                     -> (BuiltinData -> BuiltinData -> ())
wrapStakeValidator = wrapPolicy  

