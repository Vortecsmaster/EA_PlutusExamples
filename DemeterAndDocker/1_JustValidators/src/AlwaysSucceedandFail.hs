{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax

module AlwaysSucceedandFail where

--PlutusTx 
import                  PlutusTx                       (BuiltinData, compile)
import                  PlutusTx.Builtins           as Builtins (mkI)
import                  PlutusTx.Prelude               (error, otherwise, (==), Bool (..), Integer)
import                  Plutus.V2.Ledger.Api        as PlutusV2
--Serialization
import                  Serialization    (writeValidatorToFile, writeDataToFile)
import                  Prelude                     (IO)
 
--THE ON-CHAIN CODE

{-# INLINABLE alwaysSucceeds #-}                                    -- Everything that its supposed to run in on-chain code need this pragma
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()   -- the value of this function is on its sideeffects
alwaysSucceeds _ _ _ = () 

{-# INLINABLE alwaysFails #-}
alwaysFails :: BuiltinData -> BuiltinData -> BuiltinData -> ()   
alwaysFails _ _ _ = error () 

{-# INLINABLE redeemer11 #-}
redeemer11 ::BuiltinData -> BuiltinData -> BuiltinData -> () 
redeemer11 _ redeemer _ 
 | redeemer == mkI 11  = ()
 | otherwise           = error ()

{-# INLINABLE datum22 #-}
datum22 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
datum22 datum _ _ 
 | datum == mkI 22     = ()
 | otherwise           = error ()

{-# INLINABLE datum23 #-}
datum23 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
datum23 datum _ _ 
 | datum == mkI 23     = ()
 | otherwise           = error ()

{-# INLINABLE datumEqredeemer #-}
datumEqredeemer :: BuiltinData -> BuiltinData -> BuiltinData -> ()
datumEqredeemer datum redeemer _ 
 | redeemer == datum   = ()
 | redeemer == mkI 11  = ()
 | otherwise           = error ()

alwaysSucceedsValidator :: Validator
alwaysSucceedsValidator = mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])  

alwaysFailsValidator :: Validator
alwaysFailsValidator = mkValidatorScript $$(PlutusTx.compile [|| alwaysFails ||])  

redeemer11Validator :: Validator
redeemer11Validator = mkValidatorScript $$(PlutusTx.compile [|| redeemer11 ||])  

datum22Validator :: Validator
datum22Validator = mkValidatorScript $$(PlutusTx.compile [|| datum22 ||])

datum23Validator :: Validator
datum23Validator = mkValidatorScript $$(PlutusTx.compile [|| datum23 ||]) 

dErValidator :: Validator
dErValidator = mkValidatorScript $$(PlutusTx.compile [|| datumEqredeemer ||]) 



{- Serialised Scripts and Values -}

saveAlwaysSucceeds :: IO ()
saveAlwaysSucceeds =  writeValidatorToFile "./testnet/alwaysSucceeds.uplc" alwaysSucceedsValidator

saveAlwaysFails :: IO ()
saveAlwaysFails =  writeValidatorToFile "./testnet/alwaysFails.uplc" alwaysFailsValidator

saveRedeemer11 :: IO ()
saveRedeemer11 =  writeValidatorToFile "./testnet/redeemer11.uplc" redeemer11Validator

saveDatum22 :: IO ()
saveDatum22 =  writeValidatorToFile "./testnet/datum22.uplc" datum22Validator

saveDatum23 :: IO ()
saveDatum23 =  writeValidatorToFile "./testnet/datum23.uplc" datum23Validator

saveDeR :: IO ()
saveDeR =  writeValidatorToFile "./testnet/dEr.uplc" dErValidator

saveUnit :: IO ()
saveUnit = writeDataToFile "./testnet/unit.json" ()

saveTrue :: IO ()
saveTrue = writeDataToFile "./testnet/True.json" True

saveFalse :: IO ()
saveFalse = writeDataToFile "./testnet/False.json" False

saveValue11 :: IO ()
saveValue11 = writeDataToFile "./testnet/value11.json" (11 :: Integer)

saveValue22 :: IO ()
saveValue22 = writeDataToFile "./testnet/value22.json" (22 :: Integer)

saveValue23 :: IO ()
saveValue23 = writeDataToFile "./testnet/value23.json" (23 :: Integer)

saveAll :: IO ()
saveAll = do
            saveAlwaysSucceeds
            saveAlwaysFails
            saveRedeemer11
            saveDatum22
            saveDatum23
            saveUnit
            saveTrue
            saveFalse
            saveValue11
            saveValue22
            saveValue23
            saveDeR
