{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module GuessingGameV2 where

--PlutusTx 
import           PlutusTx                             (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins                    as Builtins
import           PlutusTx.Prelude                     hiding (Semigroup(..), unless)

--Contract Monad
import           Plutus.Contract               
--Ledger 
import           Ledger                               hiding (singleton, ScriptContext)
import qualified Ledger.Address                       as V1Address
import           Ledger.Constraints                   as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Scripts               as Scripts               
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions
import           Plutus.V2.Ledger.Contexts            as Contexts
import qualified Plutus.V2.Ledger.Api                 as PlutusV2            
import           Ledger.Ada                           as Ada 
--Trace Emulator
import           Plutus.Trace
import qualified Plutus.Trace.Emulator                as Emulator
import qualified Wallet.Emulator.Wallet               as Wallet
{-Plutus Playground (broken)
import           Playground.Contract            (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH                  (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types               (KnownCurrency (..))
--"Normal" Haskell -}
import           Control.Monad                        hiding (fmap)
import           GHC.Generics                         (Generic)
import           Data.Aeson                           (ToJSON, FromJSON)
import           Data.Map                             as Map
import           Data.Text                            (Text)
import           Data.Void                            (Void)
import           Prelude                              (IO, Semigroup (..), String, Show (..))
import           Text.Printf                          (printf)
import           Control.Monad.Freer.Extras           as Extras

--GuessingGame imports

-- import Data.ByteString.Char8 qualified as C
-- import Data.Maybe (catMaybes)

-- import Ledger (Address, Datum (Datum), ScriptContext, Validator, Value, getCardanoTxId)
-- import Ledger qualified
-- import Ledger.Ada qualified as Ada
-- import Ledger.Constraints qualified as Constraints
-- import Ledger.Tx (ChainIndexTxOut (..))
-- import Ledger.Typed.Scripts qualified as Scripts

-- import           Text.Printf          (printf)



{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

data MathBountyDatum = MBD 
                     { mbBounty    :: Integer
                     , mbDeadline :: POSIXTime }  

PlutusTx.makeIsDataIndexed ''MathBountyDatum [('MBD,0)]        -- At compile time write an instance of this data type (MyWonderFullRedeemer) on the IsData typeclass

{-# INLINABLE mathBountyValidator  #-}
mathBountyValidator :: MathBountyDatum -> Integer -> PlutusV2.ScriptContext -> Bool
mathBountyValidator datum x sContext = traceIfFalse "Wrong guess!" ((mbBounty datum) == x*x) &&
                                       traceIfFalse "Deadline passed!" deadlineReached
       where
            info :: Contexts.TxInfo
            info = Contexts.scriptContextTxInfo sContext

            deadlineReached :: Bool
            deadlineReached = contains (to $ mbDeadline datum) $ PlutusV2.txInfoValidRange info
                           -- (to $ mbDeadline datum) `contains` (txInfoValidRange info)

data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

bountyValidator :: Scripts.TypedValidator MathBounty
bountyValidator = Scripts.mkTypedValidator @MathBounty
    $$(PlutusTx.compile [|| mathBountyValidator ||])
    $$(PlutusTx.compile [|| wrapping ||])
     where
       wrapping  = Scripts.mkUntypedValidator @MathBountyDatum @Integer

validator :: Validator
validator = Scripts.validatorScript bountyValidator                   

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash bountyValidator                      

bountyAddress :: Ledger.Address
bountyAddress = Scripts.validatorAddress bountyValidator                 -- New function to derive the address, included in the Utils library
--bountyAddress = scriptAddress valiator 




-- module Batch50GuessingGame where

--ON-CHAIN

-- newtype HashedString = HS BuiltinByteString
--    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

-- PlutusTx.makeLift ''HashedString

-- newtype ClearString = CS BuiltinByteString
--     deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

-- PlutusTx.makeLift ''ClearString

-- data Game
-- instance Scripts.ValidatorTypes Game where
--     type instance RedeemerType Game = ClearString
--     type instance DatumType Game = HashedString

-- {-# INLINABLE validateGuess #-}
-- validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
-- validateGuess hs cs _ = isGoodGuess hs cs
 
-- {-# INLINABLE isGoodGuess #-}
-- isGoodGuess :: HashedString -> ClearString -> Bool
-- isGoodGuess (HS secret) (CS guess') = secret == sha2_256 guess'

-- gameInstance :: Scripts.TypedValidator Game
-- gameInstance = Scripts.mkTypedValidator @Game
--     $$(PlutusTx.compile [|| validateGuess ||])
--     $$(PlutusTx.compile [|| wrap ||]) where
--         wrap = Scripts.wrapValidator @HashedString @ClearString

-- --guessInstance :: Scripts.TypedValidator Game
-- --guessInstance = Scripts.mkTypedValidator @Game
-- --    $$(PlutusTx.compile [|| isGoodGuess ||])
-- --    $$(PlutusTx.compile [|| wrap ||]) where
-- --        wrap = Scripts.wrapValidator @HashedString @ClearString

-- gameValidator :: Validator
-- gameValidator = Scripts.validatorScript gameInstance

-- gameAddress :: Address
-- gameAddress = Ledger.scriptAddress gameValidator

-- --guessValidator :: Validator
-- --guessValidator = Scripts.validatorScript guessValidator

-- --guessAddress :: Address
-- --guessAddress = Ledger.scriptAddress guessValidator

-- hashString :: Haskell.String -> HashedString
-- hashString = HS . sha2_256 . toBuiltin . C.pack

-- clearString :: Haskell.String -> ClearString
-- clearString = CS . toBuiltin . C.pack

-- -- OFF-CHAIN
-- data BlindParams = BP 
--                    { theSecret :: Haskell.String
--                    , value     :: Value
--                    } 
--                    deriving stock (Haskell.Eq, Haskell.Show, Generic)
--                    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- data GuessParams = GP 
--                    { guessSecret :: Haskell.String }
--                    deriving stock (Haskell.Eq, Haskell.Show, Generic)
--                    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- type GameSchema = 
--          Endpoint "theBlind" BlindParams
--      .\/ Endpoint "theGuess" GuessParams

-- theBlind :: BlindParams -> Contract w GameSchema Text ()
-- theBlind (BP secret blind) =  do
--                         let tx         = Constraints.mustPayToTheScript (hashString secret) blind
--                         ledgerTx <- submitTxConstraints gameInstance tx
--                         void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--                         logInfo @Haskell.String $ "Pay " <> Haskell.show blind <> " to the script"    
--                         logInfo @Haskell.String $ printf "Put secret %s" secret 

-- theGuess :: GuessParams -> Contract w GameSchema Text ()
-- theGuess (GP theguess) = do
--           utxos <- utxosAt gameAddress 
--           case Map.toList utxos of
--                   []             -> logInfo @Haskell.String $ printf "No UTxOs on the Contract!"
--                   (oref,a):utxos -> do
--                                     let redeemer = clearString theguess
--                                     let lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) <>
--                                                   Constraints.otherScript gameValidator 
--                                     let tx = Constraints.mustSpendScriptOutput oref (Ledger.Redeemer $ PlutusTx.toBuiltinData redeemer) 
--                                     ledgerTx <- submitTxConstraintsWith @Void lookups tx
--                                     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--                                     logInfo @Haskell.String $ printf "Guesses correctly: %s" (Haskell.show theguess)  
