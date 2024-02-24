{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names

module AlwaysSucceedandFailV2 where

--PlutusTx 
import           PlutusTx                       (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins              as Builtins
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
--Contract Monad
import           Plutus.Contract               
--Ledger 
import           Ledger                         hiding (singleton)
import           Plutus.V1.Ledger.Address       as V1Address
--import qualified Ledger.Address                 as V1Address
import           Ledger.Constraints             as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Scripts               as Scripts               
import qualified Plutus.Script.Utils.V2.Scripts as Scripts                  -- New library name for Typed Validators and some new fuctions
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE


{-# INLINABLE alwaysSucceeds #-}                                    -- Everything that its supposed to run in on-chain code need this pragma
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()   -- the value of this function is on its sideeffects
alwaysSucceeds _ _ _ = () 

{-# INLINABLE alwaysFails #-}
alwaysFails :: BuiltinData -> BuiltinData -> BuiltinData -> ()   
alwaysFails _ _ _ = error () 

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])  

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator  

scrAddress :: V1Address.Address
scrAddress = V1Address.scriptHashAddress valHash          -- Couldn't find a new version of scriptAddress for unTyped Scripts
--replaces:
--scrAddress = scriptAddress validator 

--THE OFFCHAIN CODE

type GiftSchema =
            Endpoint "give" Integer  --An Integer Parameter
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount      --This Tx needs an output, thats its going to be the Script Address, Datum MUST be specified, so is created and the ammount of lovelaces
    ledgerTx <- submitTx tx                                                                          --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    Plutus.Contract.logInfo @String $ printf "made a gift of %d lovelace" amount                                     --This line log info,usable on the PP(Plutus Playground)
    
grab :: forall w s e. AsContractError e => Contract w s e ()                                     
grab = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                         
                                  -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.plutusV2OtherScript validator                                          -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    Plutus.Contract.logInfo @String $ "collected gifts"                                                              -- Log information 

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" $ const grab                                                            -- block until grab

-- Playground broken at the moment - September 2022
-- mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for the playground
-- mkKnownCurrencies []                                                                                 -- MakeKnown currencies for the playground to have some ADA available


--SIMULATION

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet.knownWallet 1) endpoints
    h2 <- activateContractWallet (Wallet.knownWallet 2) endpoints
    callEndpoint @"give" h1 $ 51000000
    void $ Emulator.waitNSlots 11
    callEndpoint @"grab" h2 ()
    s <- Emulator.waitNSlots 11
    Extras.logInfo $ "End of Simulation at slot " ++ show s