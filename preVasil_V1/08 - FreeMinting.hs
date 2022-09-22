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

module FreeMinting where

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

{-# INLINABLE freeMintingPolicy  #-}

freeMintingPolicy :: () -> ScriptContext -> Bool
freeMintingPolicy () _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy freeMintingPolicy  ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy


--OFF-CHAIN
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmout     :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema) 

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmout mp)
        lookups = Constraints.mintingPolicy policy
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Forged %s" (show val)

type FreeSchema = Endpoint "mint" MintParams

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

        