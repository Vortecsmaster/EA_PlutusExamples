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


module EAtokens where

--PlutusTx 
import           PlutusTx                               (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins                      as Builtins
import           PlutusTx.Prelude                       hiding (Semigroup(..), unless, (.))
--Ledger 
import           Ledger                                 hiding (singleton)
import           Plutus.V1.Ledger.Address               as V1Address
import           Plutus.V1.Ledger.Api                   as PlutusV1
import           Plutus.V2.Ledger.Api                   as PlutusV2
import           Plutus.V2.Ledger.Contexts              as LedgerV2
import           Ledger.Constraints                     as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Scripts               as Scripts               
import qualified Plutus.Script.Utils.V2.Typed.Scripts   as Scripts                  -- New library name for Typed Validators and some new fuctions
import           Ledger.Ada                             as Ada 
import           Plutus.V1.Ledger.Value                 as Value
--"Normal" Haskell -}
import           Prelude                                (IO, Semigroup (..), Show (..), print, (.))
import           Data.Aeson                             as A
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.ByteString.Short                  as SBS
import           Data.Functor                           (void)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

{-# INLINABLE asPolicy #-}
asPolicy ::  BuiltinData -> BuiltinData -> ()
asPolicy _ _ = ()

{-# INLINABLE r42Policy #-}
r42Policy ::  BuiltinData -> BuiltinData -> ()
r42Policy r _ = if r == (Builtins.mkI 42) 
                    then () 
                    else (error ())

{-# INLINABLE rightAssetMP #-}
rightAssetMP ::  Value.AssetClass -> LedgerV2.ScriptContext -> Bool
rightAssetMP asset sContext = traceIfFalse "Asset not available" findedAsset
  where
    info :: LedgerV2.TxInfo
    info = LedgerV2.scriptContextTxInfo sContext

    -- findedAsset :: Bool
    -- findedAsset =  elem asset (map (\x -> theFunct x) daList) 

    findedAsset :: Bool
    findedAsset = any (elem asset) (map (\x -> theFunct x) daList)
                --elem True (map (elem asset) (map (\x -> theFunct x) daList))
                --                            [[AssetClass]] 
    takeInputs ::  LedgerV2.TxInfo -> [LedgerV2.TxInInfo]
    takeInputs txinfo =  LedgerV2.txInfoInputs txinfo  

    takeValue ::  LedgerV2.TxOut -> Value.Value
    takeValue txOut =  LedgerV2.txOutValue txOut

    theFunct :: LedgerV2.TxInInfo -> [AssetClass]
    theFunct = daAssets . takeValue . LedgerV2.txInInfoResolved 

    daList :: [LedgerV2.TxInInfo]
    daList = takeInputs info

    daAssets :: Value.Value -> [AssetClass]
    daAssets val = map (\x -> AssetClass (showCS x, showTN x)) (flattenValue val) 

    showCS :: (CurrencySymbol, TokenName, Integer) -> CurrencySymbol
    showCS ( cs, _, _ ) = cs

    showTN :: (CurrencySymbol, TokenName, Integer) -> TokenName
    showTN ( _, tn, _ ) = tn

