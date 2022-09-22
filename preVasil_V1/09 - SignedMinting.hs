{-# LANGUAGE DataKinds           #-}
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

module SignedMinting where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

--ON-CHAIN

{-# INLINABLE signedMintingPolicy  #-}
signedMintingPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
signedMintingPolicy pkh () sContext = txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkh

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
             $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . signedMintingPolicy ||])
             `PlutusTx.applyCode`
             PlutusTx.liftCode pkh

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy


--OFF-CHAIN
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmout     :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema) 

type FreeSchema = Endpoint "mint" MintParams


mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmout mp)
        lookups = Constraints.mintingPolicy $ policy pkh
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
       h1 <- activateContractWallet (knownWallet 1) endpoints
       h2 <- activateContractWallet (knownWallet 2) endpoints
       callEndpoint @"mint" h1 $ MintParams
                        { mpTokenName = "YourBatchcoin" 
                        , mpAmout     = 1100
                        }
       void $ Emulator.waitNSlots 10
       callEndpoint @"mint" h2 $ MintParams
                        { mpTokenName = "YourBatchcoin2"
                        , mpAmout     = 2200   
                        }
       void $ Emulator.waitNSlots 10