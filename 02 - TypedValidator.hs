{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module OurGift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)
import Language.Haskell.TH (RuleBndr(TypedRuleVar))

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

{-# INLINABLE typedRedeemer #-} -- Everything that its supposed to run in on-chain code need this pragma INLINABLE
typedRedeemer :: () -> Integer -> ScriptContext -> Bool   
typedRedeemer _ redeemer _ = traceIfFalse "wrong redeemer" (redeemer == 42)
 

data Typed                                            -- New type that encode the information about the Datum and the Redeemer
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()                -- Type instances to define the type of Datum
    type instance RedeemerType Typed = Integer        -- Type instance to definte the type of Redeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed      -- Tell the compiler that you are using Types
    $$(PlutusTx.compile [|| typedRedeemer ||]) 
    $$(PlutusTx.compile [|| wrap ||])                -- Provide the translation into high level typed to low level typed
  where
    wrap = Scripts.wrapValidator @() @Integer        -- Tell wrapvalidtor which types to use for Datum and Redeemer
    

validator :: Validator
validator = Scripts.validatorScript typedValidator   -- Get the untyped validator script of the typeValidator PlutusCore

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator       -- Using Typed (Typed.Scripts) version of validatorHash we get the valHash of typedValidator script

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator 


--THE OFFCHAIN CODE

type GiftSchema =
            Endpoint "give" Integer  --
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount               -- Typed version for one script, This Tx needs an output, thats its going to be the Script Address, Datum MUST be specified, so unit ().
    ledgerTx <- submitTxConstraints typedValidator tx                                                                          --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    logInfo @String $ printf "made a gift of %d lovelace" amount                                     --This line log info,usable on the PP(Plutus Playground)
    
--grab :: forall w s e. AsContractError e => Contract w s e ()                                     
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()                                     
grab n = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "collected gifts"                                                              -- Log information 

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab
    --grab' = endpoint @"grab" $ const grab                                                            -- block until grab

mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that

mkKnownCurrencies [] 