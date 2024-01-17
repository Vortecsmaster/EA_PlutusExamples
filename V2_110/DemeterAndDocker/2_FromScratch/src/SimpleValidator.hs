{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module SimpleValidator where

import                  PlutusTx                    (BuiltinData, compile )
import                  PlutusTx.Builtins           as Builtins (mkI, emptyByteString)
import                  PlutusTx.Prelude            (error, otherwise,(==),foldr, consByteString)
import qualified        Plutus.V2.Ledger.Api        as PlutusV2
import                  Serialization    (writeValidatorToFile)

import                  Prelude                     (IO, Integer, putStr)

(p :: PubKeyHash) = "80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7"

{-# INLINABLE manager #-}
--- These magic numbers for the hardcoded 
--- PubKeyHash can be found by using  [indexByteString (getPubKeyHash p) i | i <- [0..27]] where p is the PubKeyHash in this case let (p :: PubKeyHash) = "80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7"
manager :: PlutusV2.PubKeyHash
manager = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [128,164,244,91,86,184,141,17,57,218,35,188,76,60,117,236,109,50,148,60,8,127,37,11,134,25,60,167]) }
--manage2 = PubKeyHash { getPubKeyHash =  (consByteString (consByteString emptyByteString 167) 60) ...  [128,164,244,91,86,184,141,17,57,218,35,188,76,60,117,236,109,50,148,60,8,127,37,11,134,25,60,167] }

{-# INLINABLE anaCurrencySymbol #-}
--- These magic numbers for the hardcoded CurrencySymbol can be found by using  [indexByteString (unCurrencySymbol c) i | i <- [0..27]] where c is the CurrencySymbol in this case let (c :: CurrencySymbol) = "policyIdHere"
anaCurrencySymbol :: PlutusV2.CurrencySymbol
anaCurrencySymbol = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [97,110,97]) }

{-# INLINABLE anaTokenName #-}
--- These magic numbers for the hardcoded TokenName can be found by using  [indexByteString (unTokenName tokN) i | i <- [0..( (-) (lengthOfByteString (unTokenName tokN)) 1)]] where tokN is the TokenName
--- in this case let (tokN :: TokenName) = "tokenNameHere"
anaTokenName :: PlutusV2.TokenName 
anaTokenName = PlutusV2.TokenName { PlutusV2.unTokenName = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [65,110,97,115,116,97,115,105,97]) }


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