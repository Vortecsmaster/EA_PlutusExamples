{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax

module TooSimpleValidators where

--PlutusTx 
import                  PlutusTx                       (BuiltinData, compile)
import                  PlutusTx.Builtins           as Builtins (mkI)
import                  PlutusTx.Prelude               (error, otherwise, (==), Bool (..), Integer)
import                  Plutus.V2.Ledger.Api        as PlutusV2
--Serialization
import                  Serialization    (writeValidatorToFile, writeDataToFile)
import                  Prelude                     (IO)
 
--THE ON-CHAIN CODE

{-# INLINABLE datum22 #-}
datum22 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
datum22 datum _ _ 
 | datum == mkI 22      = ()
 | otherwise           = error ()

{-# INLINABLE redeemer11 #-}
redeemer11 ::BuiltinData -> BuiltinData -> BuiltinData -> () 
redeemer11 _ redeemer _ 
 | redeemer == mkI 11   = ()
 | otherwise           = error ()

{-# INLINABLE datumVSredeemer #-} --with scape route
datumVSredeemer :: BuiltinData -> BuiltinData -> BuiltinData -> ()
datumVSredeemer datum redeemer _ 
 | datum == redeemer    = ()
 | otherwise           = error ()


redeemer11Validator :: Validator
redeemer11Validator = mkValidatorScript $$(PlutusTx.compile [|| redeemer11 ||])  

datum22Validator :: Validator
datum22Validator = mkValidatorScript $$(PlutusTx.compile [|| datum22 ||])

dVrValidator :: Validator
dVrValidator = mkValidatorScript $$(PlutusTx.compile [|| datumVSredeemer ||]) 


{- Serialised Scripts and Values -}

saveDatum22 :: IO ()
saveDatum22 =  writeValidatorToFile "./testnet/datum22.uplc" datum22Validator

saveRedeemer11 :: IO ()
saveRedeemer11 =  writeValidatorToFile "./testnet/redeemer11.uplc" redeemer11Validator

saveDvR :: IO ()
saveDvR =  writeValidatorToFile "./testnet/dVr.uplc" dVrValidator

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/values/unit.json" ()

saveTrue :: IO ()
saveTrue = writeDataToFile "./testnet/values/True.json" True

saveFalse :: IO ()
saveFalse = writeDataToFile "./testnet/values/False.json" False

saveValue11 :: IO ()
saveValue11 = writeDataToFile "./testnet/values/value11.json" (11 :: Integer)

saveValue22 :: IO ()
saveValue22 = writeDataToFile "./testnet/values/value22.json" (22 :: Integer)

saveAll :: IO ()
saveAll = do
            saveDatum22
            saveRedeemer11
            saveUnit
            saveTrue
            saveFalse
            saveValue11
            saveValue22
            saveDvR
