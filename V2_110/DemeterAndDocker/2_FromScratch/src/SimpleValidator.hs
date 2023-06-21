{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}

module SimpleValidator where

import                  PlutusTx                    (BuiltinData, compile)
import                  PlutusTx.Builtins           as Builtins (mkI)
import                  PlutusTx.Prelude            (error, otherwise,(==))
import qualified        Plutus.V2.Ledger.Api        as PlutusV2
import                  Serialization    (writeValidatorToFile)

import                  Prelude                     (IO, Integer, putStr)

{-# INLINABLE datumVsRedeemer #-}
datumVsRedeemer ::  BuiltinData -> BuiltinData -> BuiltinData -> ()
datumVsRedeemer datum redeemer _ 
 | datum == redeemer                = ()
 | otherwise                        = error ()

dvsRvalidator :: PlutusV2.Validator
dvsRvalidator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| datumVsRedeemer ||])

{-# INLINABLE datumVsRedeemer2 #-}
datumVsRedeemer2 ::  BuiltinData -> BuiltinData -> BuiltinData -> ()
datumVsRedeemer2 datum redeemer _ 
 | datum == redeemer                = ()
 | datum == Builtins.mkI 11         = ()
 | redeemer ==  Builtins.mkI 22    = ()
 | otherwise                        = error ()
--to have an error message (on a different script / scriptAddress, you can use traceError function)
dvsR2validator :: PlutusV2.Validator
dvsR2validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| datumVsRedeemer2 ||])


------------------------------------- SERIALIZE THE CONTRACT --------------------------------------------
saveDatumVsRedeemer :: IO ()
saveDatumVsRedeemer = writeValidatorToFile "./testnet/DatumVsRedeemer.plutus" dvsRvalidator

saveDatumVsRedeemer2 :: IO ()
saveDatumVsRedeemer2 = writeValidatorToFile "./testnet/DatumVsRedeemer2.plutus" dvsR2validator

saveval :: Integer -> IO ()
saveval n 
 | n == 1      = saveDatumVsRedeemer
 | n == 2      = saveDatumVsRedeemer2
 | otherwise   = putStr "...This is not the validators you are looking for..."