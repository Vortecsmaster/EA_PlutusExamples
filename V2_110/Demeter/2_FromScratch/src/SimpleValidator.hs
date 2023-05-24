{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}

module SimpleValidator where

import                  PlutusTx                    (BuiltinData, compile)
import                  PlutusTx.Builtins           as Builtins (mkI)
import                  PlutusTx.Prelude            (error, otherwise,(==),Bool)
import qualified        Plutus.V2.Ledger.Api        as PlutusV2
import                  Utils                   (writeValidatorToFile, writeDataToFile)

import                  Prelude                     (IO)

{-# INLINABLE alwaysSucceeds #-}
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

{-# INLINABLE alwaysFails #-}
alwaysFails :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysFails _ _ _ = error ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])

-- {-# INLINABLE withTypes #-}
-- withTypes ::  Integer -> String -> PlutusV2.ScriptContext -> Bool
-- withTypes _ _ _ = True

{-
  fun :: Integer -> String -> PlutusV2.ScriptContext -> Bool  =>  wrapedfun :: BuiltinData -> BuiltinData -> BuiltinData -> ()
 (String) Text -> BuiltinString  (encodeUtf8 :: BuiltinString -> BuiltinByteString) -> (B ByteString)
-}



{-# INLINABLE dvsR #-}
dvsR ::  BuiltinData -> BuiltinData -> BuiltinData -> ()
dvsR datum redeemer _ 
 | datum == redeemer                = ()
 | otherwise                        = error ()

dvsRvalidator :: PlutusV2.Validator
dvsRvalidator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| dvsR ||])

{-# INLINABLE dvsR2 #-}
dvsR2 ::  BuiltinData -> BuiltinData -> BuiltinData -> ()
dvsR2 datum redeemer _ 
 | datum == redeemer                = ()
 | datum == Builtins.mkI 11         = ()
 | redeemer ==  Builtins.mkI 22    = ()
 | otherwise                        = error ()
--to have an error message (on a different script / scriptAddress, you can use traceError function)
dvsR2validator :: PlutusV2.Validator
dvsR2validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| dvsR2 ||])

---------------------------------------------------------------------------------------------------
------------------------------------- SERIALIZE THE CONTRACT --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./testnet/DvR.plutus" dvsR2validator