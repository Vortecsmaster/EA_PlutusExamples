module Interest where


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