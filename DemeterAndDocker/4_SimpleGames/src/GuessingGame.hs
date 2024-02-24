{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-} 

module GuessingGame where

--PlutusTx 
import                  PlutusTx                            (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed, makeLift)
import                  PlutusTx.Prelude                    (traceIfFalse, otherwise, (==), Bool (..), Integer, ($), (>), (+), (&&), sha2_256)
import                  Plutus.V1.Ledger.Value          as  PlutusV1
import                  Plutus.V1.Ledger.Interval           (contains, to) 
import                  Plutus.V2.Ledger.Api            as  PlutusV2
import                  Plutus.V2.Ledger.Contexts           (txSignedBy, valueSpent)
--Serialization
import                  Wrappers                            (wrapValidator)
import                  Serialization                       (writeValidatorToFile, writeDataToFile)
import                  Prelude                             (IO,String,(.),Show (show), mapM)
--Extra
import                  Text.Printf                         (printf)
import   qualified      Data.ByteString.Char8           as  C


--THE ON-CHAIN CODE

newtype HashedString = HashedString BuiltinByteString
unstableMakeIsData ''HashedString

newtype ClearString = ClearString BuiltinByteString
unstableMakeIsData ''ClearString

{-# INLINABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs

{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: String -> ClearString
clearString = ClearString . toBuiltin . C.pack

mappedValidateGuess :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mappedValidateGuess = wrapValidator validateGuess

guessingGameValidator :: Validator
guessingGameValidator =  PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedValidateGuess ||])

-- {- Serialised Scripts and Values -}

saveValidator  :: IO ()
saveValidator  =  writeValidatorToFile "./testnet/guessingGame.plutus" guessingGameValidator

saveDatum :: IO ()
saveDatum = writeDataToFile "./testnet/secret.json" $ hashString "secretWord"

saveRedeemer :: IO ()
saveRedeemer = writeDataToFile "./testnet/guess.json" $ clearString "secretWord"  

saveAll :: IO ()
saveAll = do
            saveValidator
            saveDatum
            saveRedeemer

