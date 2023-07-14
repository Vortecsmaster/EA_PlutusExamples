{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-} 

module RockPaperScissors where

--PlutusTx 
import                  PlutusTx                            (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed, makeLift)
import                  PlutusTx.Prelude                    (Eq ,traceIfFalse, otherwise, (==), Bool (..), Integer, ($), (>), (+), (&&), sha2_256)
import                  Plutus.V1.Ledger.Value          as  PlutusV1
import                  Plutus.V1.Ledger.Interval           (contains, to) 
import                  Plutus.V2.Ledger.Api            as  PlutusV2
import                  Plutus.V2.Ledger.Contexts           (txSignedBy, valueSpent)
--Serialization
import                  Wrappers                             (wrapValidator)
import                  Serialization                       (writeValidatorToFile, writeDataToFile)
import                  Prelude                             (IO,String,(.),Show (show), mapM)
--Extra
import                  Text.Printf                         (printf)
import   qualified      Data.ByteString.Char8           as  C


--THE ON-CHAIN CODE
data GameOption = Rock | Paper | Scissors --deriving Show for REPL
data GameState = Player1 | Player2 | Draw --deriving Show for REPL

unstableMakeIsData ''GameOption
unstableMakeIsData ''GameState

instance Eq GameOption where
    {-# INLINABLE (==) #-}
    Rock     == Rock     = True
    Paper    == Paper    = True
    Scissors == Scissors = True
    _        == _        = False

{-# INLINABLE gameState #-}
gameState :: GameOption -> GameOption -> GameState
gameState p1 p2 
 | p1 == Rock && p2 == Rock             = Draw
 | p1 == Paper && p2 == Paper           = Draw
 | p1 == Scissors && p2 == Scissors     = Draw
 | p1 == Rock && p2 == Scissors         = Player1
 | p1 == Paper && p2 == Rock            = Player1
 | p1 == Scissors && p2 == Paper        = Player1
 | p1 == Rock && p2 == Paper            = Player2
 | p1 == Paper && p2 == Scissors        = Player2
 | p1 == Scissors && p2 == Rock         = Player2

 


 
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

