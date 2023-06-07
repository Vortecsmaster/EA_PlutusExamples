{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE OverloadedStrings   #-}  --Enable passing strings as other character formats, like bytestring.

module TypedValidators where

--PlutusTx 
import                  PlutusTx                       (BuiltinData, compile,unstableMakeIsData, makeIsDataIndexed)
import                  PlutusTx.Prelude               (traceIfFalse, otherwise, (==), Bool (..), Integer, ($))
import                  Plutus.V2.Ledger.Api        as PlutusV2
import                  Mappers                        (wrapValidator)
import                  Serialization                  (writeValidatorToFile, writeDataToFile)

import                  Prelude                     (IO)

------------------------------------------------------------------------------------------
-- Primites Types
------------------------------------------------------------------------------------------

{-# INLINABLE typedDatum22 #-}
typedDatum22 :: Integer -> () -> () -> Bool
typedDatum22 datum _ _ = traceIfFalse "Not the right datum!" (datum ==  22)

{-# INLINABLE typedRedeemer11 #-}
typedRedeemer11 ::  () -> Integer -> () -> Bool
typedRedeemer11 _ redeemer_ = traceIfFalse "Not the right redeemer!" (redeemer ==  11)

------------------------------------------------------------------------------------------
-- Custom Types
------------------------------------------------------------------------------------------

newtype OurWonderfullDatum = OWD Integer
unstableMakeIsData ''OurWonderfullDatum

data OurWonderfullRedeemer = OWR Integer | JOKER 
makeIsDataIndexed ''OurWonderfullRedeemer [('OWR,0),('JOKER,1)]

{-# INLINABLE customTypedDatum22 #-}
customTypedDatum22 :: Integer -> () -> () -> Bool
customTypedDatum22 datum _ _ = traceIfFalse "Not the right datum!" (datum ==  22)

{-# INLINABLE customTypedRedeemer11 #-}
customTypedRedeemer11 ::  () -> Integer -> () -> Bool
customTypedRedeemer11 _ redeemer _ = traceIfFalse "Not the right redeemer!" (redeemer ==  11)

------------------------------------------------------------------------------------------
-- Mappers and Compiling expresions
------------------------------------------------------------------------------------------

-- for primitives types

mappedTypedDatum22 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mappedTypedDatum22 = wrapValidator customTypedDatum22

typedDatum22Val :: PlutusV2.Validator
typedDatum22Val = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedTypedDatum22 ||])

mappedTypedRedeemer11 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mappedTypedRedeemer11 = wrapValidator typedRedeemer11

typedRedeemer11Val :: Validator
typedRedeemer11Val = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedTypedRedeemer11 ||])


-- for custom types

mappedCustomTypedDatum22 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mappedCustomTypedDatum22 = wrapValidator customTypedDatum22

mappedCustomRedeemer11 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mappedCustomRedeemer11 = wrapValidator customTypedRedeemer11

customTypedDatum22Val :: Validator
customTypedDatum22Val = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedCustomTypedDatum22 ||])

customTypedRedeemer11Val :: Validator
customTypedRedeemer11Val = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedCustomTypedRedeemer11 ||])