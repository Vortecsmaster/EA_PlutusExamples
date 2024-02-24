{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-} 
{-# LANGUAGE DeriveAnyClass      #-} 


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
import                  Prelude                             hiding (Eq,(==),(&&),($))
--Extra
import                  Text.Printf                         (printf)
import   qualified      Data.ByteString.Char8           as  C
import Data.Time.Clock.POSIX (POSIXTime)


--THE ON-CHAIN CODE
type Player = Integer
data GameOption = Rock | Paper | Scissors --deriving Show for REPL
type Salt = BuiltinByteString
data Secret = Secret BuiltinByteString  
data GameState = Player1 | Player2 | Reveal --deriving Show for REPL
data GameOutcome = Player1wins | Player2wins | Draw
data GameDatum = GD { player1    :: PubKeyHash
                    , player2    :: PubKeyHash
                    , p1Secret   :: Secret
                    , p1option   :: GameOption
                    , p2option   :: GameOption
                    , gametime   :: POSIXTime
                    , revealtime :: POSIXTime
                    , theBet     :: Integer
                    , gameState  :: GameState
                    }
data Action = GameChoice Player | P1claim | P2claim | DrawClaim deriving (Eq)

nOption :: GameOption -> BuiltinByteString
nOption option = case option of  
                   Rock     -> "1100"
                   Paper    -> "3300"
                   Scissors -> "5500"

unstableMakeIsData ''GameOutcome
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
gameState :: GameOption -> GameOption -> GameOutcome
gameState p1 p2 
 | p1 == Rock && p2 == Rock             = Draw
 | p1 == Paper && p2 == Paper           = Draw
 | p1 == Scissors && p2 == Scissors     = Draw
 | p1 == Rock && p2 == Scissors         = Player1wins
 | p1 == Paper && p2 == Rock            = Player1wins
 | p1 == Scissors && p2 == Paper        = Player1wins
 | p1 == Rock && p2 == Paper            = Player2wins
 | p1 == Paper && p2 == Scissors        = Player2wins
 | p1 == Scissors && p2 == Rock         = Player2wins

{-# INLINABLE rpsValidator #-}
rpsValidator :: GameDatum -> Action -> ScriptContext -> Bool
rpsValidator datum action sContext 
 | action == (GameChoice 2)  = gameChoice 2 
 | action == (GameChoice 1)  = gameChoice 1 
 | action == P1claim       = True --p1claim
 | action == P2claim       = True --p2claim
 | action == DrawClaim     = True --Draw
 | otherwise               = False
    where
        gameChoice :: Integer -> Bool
        gameChoice 2 = True 
        gameChoice 1 = True    

        -- findUTxOdatum :: OutputDatum
        -- findUTxOdatum = case findOwnInput sContext of
        --     Just utxo    ->  txOutDatum $ txInInfoResolved utxo
        --     Nothing      ->  NoOutputDatum 
            
        -- Current UTxO been consumed DATUM must have the gameState == Player1
        --if (p1Secret datum /= "") then player2choice else somethingElse
        





        player2choice :: Bool
        player2choice = 
            
            traceIfFalse "Must be signed by Player!" 
            
            True --(signedBy $ player2 datum) && 
                        --verifyProvidedValue &&
                        --verifyOutputPlayer2 

        checkGameState :: GameState -> Bool    
        checkGameState state = True

        signedBy :: PubKeyHash -> Bool
        signedBy pkh = txSignedBy info pkh
 
        info :: TxInfo
        info = scriptContextTxInfo sContext
{-
{-# INLINABLE parseOracleDatum #-}
parseOracleDatum :: TxOut -> TxInfo -> Maybe Integer
parseOracleDatum o info = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
    OutputDatumHash dh -> do
                        Datum d <- findDatum dh info
                        PlutusTx.fromBuiltinData d    
-}       

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