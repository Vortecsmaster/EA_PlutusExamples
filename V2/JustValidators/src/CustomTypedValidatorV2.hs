{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module CustomTypedValidatorV2 where

--PlutusTx 
import           PlutusTx                       (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins              as Builtins
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
--Contract Monad
import           Plutus.Contract               
--Ledger 
import           Ledger                         hiding (singleton)
import qualified Ledger.Address                 as V1Address
import           Ledger.Constraints             as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Typed.Scripts         as Scripts              
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions
import qualified Plutus.V2.Ledger.Api                 as PlutusV2           -- 
import           Ledger.Ada                     as Ada
--Trace Emulator
import           Plutus.Trace
import qualified Plutus.Trace.Emulator          as Emulator
import qualified Wallet.Emulator.Wallet         as Wallet
{-Plutus Playground (broken)
import           Playground.Contract            (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH                  (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types               (KnownCurrency (..))
--"Normal" Haskell -}
import           Control.Monad                  hiding (fmap)
import           Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Prelude                        (IO, Semigroup (..), String, show)
import           Text.Printf                    (printf)
import           Control.Monad.Freer.Extras     as Extras

{-
import Language.Haskell.TH (RuleBndr(TypedRuleVar))
-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

newtype MyWonderfullRedeemer = MWR Integer

PlutusTx.makeIsDataIndexed ''MyWonderfullRedeemer [('MWR,0)]        -- At compile time write an instance of this data type (MyWonderFullRedeemer) on the IsData typeclass

{-# INLINABLE typedRedeemer #-} 
typedRedeemer :: () -> MyWonderfullRedeemer -> PlutusV2.ScriptContext -> Bool   
typedRedeemer _ (MWR redeemer) _ = traceIfFalse "Wrong redeemer!" (redeemer == 42)
 

data Typed                                                          -- New type that encode the information about the Datum and the Redeemer
instance Scripts.ValidatorTypes Typed where
     type instance RedeemerType Typed = MyWonderfullRedeemer
     type instance DatumType Typed = ()

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed      
    $$(PlutusTx.compile [|| typedRedeemer ||]) 
    $$(PlutusTx.compile [|| wrap ||])                               
  where
    wrap = Scripts.mkUntypedValidator  @() @MyWonderfullRedeemer     --New wrapper function for typed validators             
    

validator :: Validator
validator = Scripts.validatorScript typedValidator                   -- Get the untyped validator script of the wrapped typeValidator PlutusCore

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator                      

scrAddress :: Ledger.Address
scrAddress = Scripts.validatorAddress typedValidator                 -- New functino to derive the address, included in the Utils library
--scrAddress = scriptAddress validator 


--THE OFFCHAIN CODE

type GiftSchema =
            Endpoint "give" Integer  --
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount               -- Typed version for one script, This Tx needs an output, thats its going to be the Script Address, Datum MUST be specified, so unit ().
    ledgerTx <- submitTxConstraints typedValidator tx                                                                          --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    Plutus.Contract.logInfo @String $ printf "made a gift of %d lovelace" amount                                     --This line log info,usable on the PP(Plutus Playground)
    
--grab :: forall w s e. AsContractError e => Contract w s e ()                                     
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()                                     
grab n = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.plutusV2OtherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData (MWR n) | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    Plutus.Contract.logInfo @String $ "collected gifts"                                                              -- Log information 

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab                                                                    -- block until grab

-- Playground broken at the moment - September 2022
-- mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for the playground
-- mkKnownCurrencies [] 

--SIMULATION

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet.knownWallet 1) endpoints
    h2 <- activateContractWallet (Wallet.knownWallet 2) endpoints
    callEndpoint @"give" h1 $ 51000000
    void $ Emulator.waitNSlots 11
    callEndpoint @"grab" h2 42
    s <- Emulator.waitNSlots 11
    Extras.logInfo $ "End of Simulation at slot " ++ show s