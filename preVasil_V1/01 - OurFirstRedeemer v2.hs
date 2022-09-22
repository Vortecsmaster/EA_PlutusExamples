{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module OurGoodRedeemer where

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
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

{-# INLINABLE goodRedeemer #-} 
goodRedeemer :: BuiltinData -> BuiltinData -> BuiltinData -> ()
goodRedeemer _ redeemer _ 
 | redeemer == Builtins.mkI 42 = ()
 | otherwise                   = traceError "Wrong Redeemer"


validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| goodRedeemer ||]) -- change between validator according to example:  alwaysSucceeds alwaysFails goodRedeemer

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator  -- just the hash of the validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator --You apply scriptAddress to the validator and you get the script address on the blockchain



--THE OFFCHAIN CODE

type GiftSchema =
            Endpoint "give" Integer  
        .\/ Endpoint "grab" Integer  -- goodRedeemer validator requires Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount      --This Tx needs an output, thats its going to be the Script Address, Datum MUST be specified, so is created and the ammount of lovelaces
    ledgerTx <- submitTx tx                                                                          --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    logInfo @String $ printf "made a gift of %d lovelace" amount                                     --This line log info, usable on the PP(Plutus Playground)
    
--grab endpoint now now takes the Integer Input                                   
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()                              --w= The Contract  s=Schema  e=Error or value of type a                                 
grab n = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer 
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "collected gifts"                                                              -- Log information 

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab
                                                             

mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that

mkKnownCurrencies []                                                                                 -- MakeKnown currencies for the playground to have some ADA available