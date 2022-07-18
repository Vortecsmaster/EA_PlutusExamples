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

module NFTMinting where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.Map               as Map
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
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

--ON-CHAIN

{-# INLINABLE nftMintingPolicy  #-}

nftMintingPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
nftMintingPolicy oref tname () sContext = traceIfFalse "UTxO not consumed"    hasUTxO            &&
                                          traceIfFalse "Wrong ammount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo sContext

    hasUTxO :: Bool
    hasUTxO = any (\utxo -> txInInfoOutRef utxo == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tname', amount)] -> tname' == tname && amount == 1
        _                     -> False
        

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tname = mkMintingPolicyScript $
             $$(PlutusTx.compile [|| \oref' tname' -> Scripts.wrapMintingPolicy $ nftMintingPolicy oref' tname' ||])
             `PlutusTx.applyCode`
             PlutusTx.liftCode oref
             `PlutusTx.applyCode`
             PlutusTx.liftCode tname

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tname = scriptCurrencySymbol $ policy oref tname


--OFF-CHAIN
data NFTParams = NFTParams
    { npTokenName   :: !TokenName
    , npAddress     :: !Address
    } deriving (Generic, ToJSON, FromJSON, Show) 

type NFTSchema = Endpoint "mint" NFTParams


mint :: NFTParams -> Contract w NFTSchema Text ()
mint nparams = do
    utxos <- utxosAt $ npAddress nparams
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tname      = npTokenName nparams
            let val     = Value.singleton (curSymbol oref tname) tname 1
                lookups = Constraints.mintingPolicy (policy oref tname) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
       let w1 = knownWallet 1
           w2 = knownWallet 2
       h1 <- activateContractWallet w1 endpoints
       h2 <- activateContractWallet w2 endpoints
       callEndpoint @"mint" h1 $ NFTParams
                        { npTokenName = "YourBatchcoin" 
                        , npAddress     = mockWalletAddress w1
                        }
       void $ Emulator.waitNSlots 10
       callEndpoint @"mint" h2 $ NFTParams
                        { npTokenName = "YourBatchcoin2"
                        , npAddress   = mockWalletAddress w2   
                        }
       void $ Emulator.waitNSlots 10
       