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


{-# INLINABLE datum22 #-}
datum22 :: Integer -> () -> () -> ()
datum22 datum _ _ = traceIfFalse "Not the right datum!" (datum ==  22)