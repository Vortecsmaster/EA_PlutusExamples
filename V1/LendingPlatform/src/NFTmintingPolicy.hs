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

module NFTmintingPolicy where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Map               as Map  hiding (filter)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)

import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator

import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins   as Builtins

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

import qualified Common.Utils             as U


--ON-CHAIN
{-# INLINABLE mkPolicy #-}
mkPolicy :: Bool -> TxOutRef -> ScriptContext -> Bool
mkPolicy isLender utxo ctx = case mintedValue of
    [(_cs, tn, n)] -> validateMint tn n
    _              -> False
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (scriptContextTxInfo ctx)

    mintedValue :: [(CurrencySymbol, TokenName, Integer)]
    mintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) mintFlattened

    calculateTokenNameHash :: BuiltinByteString
    calculateTokenNameHash =
      sha2_256 (consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

    validateTokenName :: TokenName -> Bool
    validateTokenName tn = unTokenName tn == calculateTokenNameHash

    checkForOverflow :: Bool
    checkForOverflow = txOutRefIdx utxo < 256

    validateMint :: TokenName -> Integer -> Bool
    validateMint tn amount =
      U.hasUTxO utxo ctx &&
      amount == 1 &&
      True && --validateTokenName tn 
      checkForOverflow ||
      amount == (-1)

policy :: Bool -> Scripts.MintingPolicy
policy isLender = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode isLender

plutusScript :: Bool -> Script
plutusScript = unMintingPolicyScript . policy

validator :: Bool -> Validator
validator = Validator . plutusScript

curSymbol :: Bool -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy 

--OFF-CHAIN
data MintParams = MintParams
                { mpTokenName :: !TokenName
                , mpAmount    :: !Integer
                , mpAddress   :: !Address
                } deriving (Generic, ToJSON, FromJSON) --, ToSchema

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    utxos <- utxosAt $ mpAddress mp
    case Map.keys utxos of
                 []         -> Contract.logError @String "No UTxO found on the provied Address!" 
                 oref : _   -> do
                            let val      = Value.singleton (curSymbol True) (mpTokenName mp) (mpAmount mp)
                                lookups  = Constraints.mintingPolicy $ policy True
                                tx       = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData oref) val
                            ledgerTx <- submitTxConstraintsWith @Void lookups tx
                            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                            Contract.logInfo @String $ printf "We forged %s" (show val)

type FreeSchema = Endpoint "mint" MintParams

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

-- mkSchemaDefinitions ''FreeSchema
-- mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
       let w1 = knownWallet 1
           w2 = knownWallet 2
       h1 <- activateContractWallet (w1) endpoints
       h2 <- activateContractWallet (w2) endpoints
       callEndpoint @"mint" h1 $ MintParams
                        { mpTokenName = "Batch80borrowerNFT" 
                        , mpAmount     = 1
                        , mpAddress    = mockWalletAddress w1
                        }
       callEndpoint @"mint" h2 $ MintParams
                        { mpTokenName = "Batch80lenderNFT"
                        , mpAmount     = 1
                        , mpAddress   = mockWalletAddress w2   
                        }
       void $ Emulator.waitNSlots 10
       

