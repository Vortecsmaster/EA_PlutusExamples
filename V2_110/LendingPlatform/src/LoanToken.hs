{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module LoanToken where

--PlutusTx 
import           PlutusTx                       (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins              as Builtins
import           PlutusTx.Prelude               hiding (Semigroup(..), unless, (.))
           
--Ledger 
import           Ledger                         hiding (singleton)
import qualified Ledger.Address                 as V1Address
import           Ledger.Constraints             as Constraints              -- Same library name, different functions for V1 and V2 in some cases
import           Ledger.Typed.Scripts           as UScripts
import           Ledger.Typed.Scripts.Validators
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions
import qualified Plutus.V2.Ledger.Api                 as PlutusV2            
import           Ledger.Ada                     as Ada
--import for Serialization
import           Cardano.Api                          (PlutusScript, PlutusScriptV2, writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import          Codec.Serialise          
import           Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Prelude                              (FilePath, IO, Semigroup (..), Show (..), print, (.))
import qualified Plutus.V1.Ledger.Api                 as PlutusV1


data TokenUser = Borrower | Lender | DaBot deriving Eq
PlutusTx.makeIsDataIndexed ''TokenUser0 [('Borrower,0),('Lender,1),('DaBot,2)]

{-# INLINABLE userToken #-} 
userToken :: TokenUser -> PlutusV2.ScriptContext -> Bool
userToken _ _ = True
