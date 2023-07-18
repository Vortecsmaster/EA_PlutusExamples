{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-} 

module RockPaperScissors where

--PlutusTx 
import                  PlutusTx                            (BuiltinData, compile, unstableMakeIsData, makeIsDataIndexed, makeLift)
import                  PlutusTx.Prelude                    (appendByteString, Eq ,traceIfFalse, otherwise, (==), Bool (..), Integer, ($), (>), (+), (&&), sha2_256)
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
type Salt = BuiltinByteString
data Secret = Secret BuiltinByteString  
data GameState = Player1 | Player2 | Draw --deriving Show for REPL
data GameDatum = GD { player1   :: PubKeyHash
                    , player2   :: PubKeyHash
                    , p1Secret  :: Secret
                    , p1option  :: GameOption
                    , p2option  :: GameOption
                    , theBet    :: Integer
                    }
data Action = GameChoice | P1claim | P2claim | DrawClaim

nOption :: GameOption -> BuiltinByteString
nOption option = case option of  
                   Rock     -> "1100"
                   Paper    -> "3300"
                   Scissors -> "5500"



unstableMakeIsData ''GameOption
unstableMakeIsData ''Secret
unstableMakeIsData ''GameState
unstableMakeIsData ''GameDatum
unstableMakeIsData ''Action

hashSecret :: Salt -> GameOption -> Secret
hashSecret salt option = Secret $ sha2_256 $ salt `appendByteString` (nOption option) 


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

{-# INLINABLE rpsValidator #-}
rpsValidator :: GameDatum -> Action -> ScriptContext -> Bool
rpsValidator datum action sContext 
 | action == GameChoice    = gameChoice
 | action == P1claim       = p1claim
 | action == P2claim       = p2claim
 | action == DrawClaim     = Draw
 | otherwise               = False
    where
        gameChoice :: Bool
        gameChoice = if (p1Secret datum /= "") then player2choice else somethingElse

        player2choice :: Bool
        player2choice = (signedBy $ player2 datum) && 
                        verifyProvidedValue &&
                        verifyOutputPlayer2 


        signedBy :: PubKeyHash -> Bool
        signedBy pkh = txSignedBy info pkh
 
        info :: TxInfo
        info = scriptContextTxInfo sContext
        

-- mappedValidateGuess :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mappedValidateGuess = wrapValidator validateGuess

-- guessingGameValidator :: Validator
-- guessingGameValidator =  PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mappedValidateGuess ||])

-- -- {- Serialised Scripts and Values -}

-- saveValidator  :: IO ()
-- saveValidator  =  writeValidatorToFile "./testnet/guessingGame.plutus" guessingGameValidator

-- saveDatum :: IO ()
-- saveDatum = writeDataToFile "./testnet/secret.json" $ hashString "secretWord"

-- saveRedeemer :: IO ()
-- saveRedeemer = writeDataToFile "./testnet/guess.json" $ clearString "secretWord"  

-- saveAll :: IO ()
-- saveAll = do
--             saveValidator
--             saveDatum
--             saveRedeemer