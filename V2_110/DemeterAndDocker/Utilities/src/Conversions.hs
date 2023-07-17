{-# LANGUAGE GADTs #-}

module Conversions
  ( bytesFromHex
  , bytesToHex

  ) where

import qualified Cardano.Api                 as Api
import           Cardano.Api.Shelley         (Address (..))
import qualified Cardano.Api.Shelley         as Api
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.BaseTypes    (CertIx (..), Network (..),
                                              TxIx (..))
import           Cardano.Ledger.Credential   as Ledger
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as BS16
import           Data.Text                   (pack)
import qualified Data.Text                   as Text
import qualified Data.Time.Clock.POSIX       as Time
import qualified Data.Time.Format.ISO8601    as Time
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V2.Ledger.Api        (CurrencySymbol (CurrencySymbol),
                                              MintingPolicy,
                                              MintingPolicyHash (MintingPolicyHash),
                                              POSIXTime, Validator, ValidatorHash (ValidatorHash))
import qualified Plutus.V2.Ledger.Api        as Plutus
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..))


bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = either error id . BS16.decode

bytesToHex :: BS.ByteString -> BS.ByteString
bytesToHex = BS16.encode
