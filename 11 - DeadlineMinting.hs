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

--NEW EXAMPLE CREATE WITH BATCH50 on live session (Copyright BATCH50 XD) 
module DeadlineMinting where 

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Ada             as Ada
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Plutus.V1.Ledger.Time   as Time(POSIXTime(..))

--ON-CHAIN

{-# INLINABLE deadlineMintingPolicy #-}
deadlineMintingPolicy :: POSIXTime -> PaymentPubKeyHash -> () -> ScriptContext -> Bool
deadlineMintingPolicy ptime pkh () sContext = traceIfFalse "Can't no longer mint"  deadlineNotReached &&
                                              traceIfFalse "No proper signature" ifSigned
                                            
 where
   info :: TxInfo
   info = scriptContextTxInfo sContext

   deadlineNotReached :: Bool
   deadlineNotReached = contains (to ptime) $ txInfoValidRange info
 
   ifSigned :: Bool
   ifSigned = txSignedBy info $ unPaymentPubKeyHash pkh


policy :: POSIXTime -> PaymentPubKeyHash -> Scripts.MintingPolicy
policy ptime pkh = mkMintingPolicyScript $
           $$(PlutusTx.compile [|| \ptime' pkh'-> Scripts.wrapMintingPolicy $ deadlineMintingPolicy ptime' pkh' ||])
           `PlutusTx.applyCode`
           PlutusTx.liftCode ptime
           `PlutusTx.applyCode` 
           PlutusTx.liftCode pkh


curSymbol :: POSIXTime -> PaymentPubKeyHash -> CurrencySymbol
curSymbol ptime pkh = scriptCurrencySymbol $ policy ptime pkh


--OFF-CHAIN
data MintParams = MintParams
     { mpTokenName :: !TokenName
     , mpAmount    :: !Integer 
     } deriving (Generic, ToJSON, FromJSON, ToSchema)


mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    let ptime = POSIXTime (1596059111000 :: Integer)  -- if 0 the 4 of them mint
    now <- currentTime
    pkh <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton (curSymbol ptime pkh) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy ptime pkh
        tx      = Constraints.mustMintValue val <> Constraints.mustValidateIn (to now)
    ledgerTx  <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Batch 50 Sign and Forged %s" (show val)

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
                    { mpTokenName = "batchNcoin"
                    , mpAmount    = 1000
                    }
    void $ Emulator.waitNSlots 10
    callEndpoint @"mint" h2 $ MintParams
                    { mpTokenName = "batchNcoin2"
                    , mpAmount    = 2000
                    }
    void $ Emulator.waitNSlots 11
    callEndpoint @"mint" h1 $ MintParams
                    { mpTokenName = "batchNcoin3"
                    , mpAmount    = 3000
                    }
    void $ Emulator.waitNSlots 10
    callEndpoint @"mint" h2 $ MintParams
                    { mpTokenName = "batchNcoin4"
                    , mpAmount    = 4000
                    }
    void $ Emulator.waitNSlots 11
    
