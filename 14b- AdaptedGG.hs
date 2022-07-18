{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Batch50GuessingGame where

import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Text                  (Text)
import Data.Void
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Ledger (Address, Datum (Datum), ScriptContext, Validator, Value, getCardanoTxId)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))
import Prelude (Semigroup (..))
import Prelude qualified as Haskell
import           Text.Printf          (printf)

--ON-CHAIN

newtype HashedString = HS BuiltinByteString
   deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = CS BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

{-# INLINABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = isGoodGuess hs cs
 
{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HS secret) (CS guess') = secret == sha2_256 guess'

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @HashedString @ClearString

--guessInstance :: Scripts.TypedValidator Game
--guessInstance = Scripts.mkTypedValidator @Game
--    $$(PlutusTx.compile [|| isGoodGuess ||])
--    $$(PlutusTx.compile [|| wrap ||]) where
--        wrap = Scripts.wrapValidator @HashedString @ClearString

gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

--guessValidator :: Validator
--guessValidator = Scripts.validatorScript guessValidator

--guessAddress :: Address
--guessAddress = Ledger.scriptAddress guessValidator

hashString :: Haskell.String -> HashedString
hashString = HS . sha2_256 . toBuiltin . C.pack

clearString :: Haskell.String -> ClearString
clearString = CS . toBuiltin . C.pack

-- OFF-CHAIN
data BlindParams = BP 
                   { theSecret :: Haskell.String
                   , value     :: Value
                   } 
                   deriving stock (Haskell.Eq, Haskell.Show, Generic)
                   deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data GuessParams = GP 
                   { guessSecret :: Haskell.String }
                   deriving stock (Haskell.Eq, Haskell.Show, Generic)
                   deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

type GameSchema = 
         Endpoint "theBlind" BlindParams
     .\/ Endpoint "theGuess" GuessParams

theBlind :: BlindParams -> Contract w GameSchema Text ()
theBlind (BP secret blind) =  do
                        let tx         = Constraints.mustPayToTheScript (hashString secret) blind
                        ledgerTx <- submitTxConstraints gameInstance tx
                        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                        logInfo @Haskell.String $ "Pay " <> Haskell.show blind <> " to the script"    
                        logInfo @Haskell.String $ printf "Put secret %s" secret 

theGuess :: GuessParams -> Contract w GameSchema Text ()
theGuess (GP theguess) = do
          utxos <- utxosAt gameAddress 
          case Map.toList utxos of
                  []             -> logInfo @Haskell.String $ printf "No UTxOs on the Contract!"
                  (oref,a):utxos -> do
                                    let redeemer = clearString theguess
                                    let lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) <>
                                                  Constraints.otherScript gameValidator 
                                    let tx = Constraints.mustSpendScriptOutput oref (Ledger.Redeemer $ PlutusTx.toBuiltinData redeemer) 
                                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                    logInfo @Haskell.String $ printf "Guesses correctly: %s" (Haskell.show theguess)  

endpoints :: Contract () GameSchema Text ()
endpoints = awaitPromise (theBlind' `select` theGuess') >> endpoints
   where
     theBlind' = endpoint @"theBlind" theBlind
     theGuess' = endpoint @"theGuess" theGuess

mkSchemaDefinitions ''GameSchema
mkKnownCurrencies []
