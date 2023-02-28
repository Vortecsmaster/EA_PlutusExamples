{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module EAnft 
  ( printRedeemer,
    serialisedScript,
    scriptSBS,
    script,
    createTokenSc,
  )
where

--PlutusTx 
import           PlutusTx                               (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins                      as Builtins
import           PlutusTx.Prelude                       hiding (Semigroup(..), unless, (.))
           
--Ledger 
import           Ledger                                 hiding (singleton)
import           Ledger.Typed.Scripts                   as MPScripts
import           Plutus.V1.Ledger.Address               as V1Address
import           Plutus.V1.Ledger.Api                   as PlutusV1
import           Plutus.V2.Ledger.Api                   as PlutusV2
import           Plutus.V2.Ledger.Contexts              as PlutusV2
import           Ledger.Constraints                     as Constraints              
import           Plutus.Script.Utils.V2.Typed.Scripts   as Scripts
import           Ledger.Ada                             as Ada 
import           Ledger.Value                           as Value
--"Normal" Haskell for serialization -}
import           Prelude                                (IO, Semigroup (..), Show (..), print, (.))
import           Data.Aeson                             as A
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.ByteString.Short                  as SBS
import           Data.Functor                           (void)
import           Codec.Serialise
import           Cardano.Api                          (writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       PlutusScriptV2,
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

data NFTParams = NFTParams --  doesn't need more than the TxOutRef
    { --mpTokenName :: !Plutus.TokenName
      mpAmount   :: !Integer
    , mpTxOutRef :: !PlutusV2.TxOutRef
    --, mpPubKeyHs  :: !Plutus.PubKeyHash
    } deriving Show

PlutusTx.makeLift ''NFTParams
PlutusTx.unstableMakeIsData ''NFTParams

redeemer :: NFTParams
redeemer = NFTParams { mpAmount = 1,
                       mpTxOutRef = PlutusV2.TxOutRef {txOutRefId = "3367d2774baf8a4a9318e7c87070e5ac970623d4a0f0d6d1bc54058085021396"
                     , txOutRefIdx = 0}
                     }

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)

{-# INLINABLE mkPolicy #-}
mkPolicy :: NFTParams -> BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy p _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                   traceIfFalse "wrong amount minted" checkNFTAmount

  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> PlutusV2.txInInfoOutRef i == mpTxOutRef p) $ PlutusV2.txInfoInputs info

    checkNFTAmount :: Bool
    checkNFTAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
       [(cs, tn', amt)] -> cs  == PlutusV2.ownCurrencySymbol ctx && amt == 1
       _                -> False

{- Compile into UPLC-}

policy :: NFTParams -> Scripts.MintingPolicy
policy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp
  where
    wrap mp' = MPScripts.mkUntypedMintingPolicy $ mkPolicy mp'

{- As a Script -}

script :: PlutusV2.Script 
script = PlutusV2.unMintingPolicyScript $ policy redeemer

{- As a Short Byte String -}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

{- As a Serialised Script -}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

createTokenSc :: IO ()
createTokenSc = void $ writeFileTextEnvelope "testnet/EAtokenV2.plutus" Nothing serialisedScript