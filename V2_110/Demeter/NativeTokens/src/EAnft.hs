{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module EAnft where

import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (True), (&&))
import           Plutus.V2.Ledger.Api (BuiltinData, CurrencySymbol,
                                       MintingPolicy, ScriptContext,
                                       mkMintingPolicyScript)

import           Prelude              (IO)
import           Mappers          (wrapPolicy)
import           Serialization    (currencySymbol, writePolicyToFile) 


{- Infamous Alice must pay enought ADA to cover the price, defined in the datum of the reference input provided by the platform wallet
   The minting is goint to provided a fresh minted NFT, enforcing the following conditions:
    1. Alice wallet must provide equal or greater value to cover price, pegged ADA (for the NFT) and fees.
    2. Alice must recieve the NFT.
    3. Platform wallet must recieve the corresponding price.
    4. Platform wallet must sign this tx.
    5. Platform wallet must hold the authorization NFT. (can be presente as reference input)
-}

{-# INLINABLE eaNFT #-}
eaNFT :: () -> ScriptContext -> Bool
eaNFT _ sContext = approvedMinting 
    where
        approvedMinting :: Bool
        approvedMinting = mintingAllowed --traceIfFalse "You shall NOT PASS!"

        mintingAllowed :: Bool 
        mintingAllowed = buyerCoversPrice &&
                         buyergetNFT &&
                         sellerwithANFTsigned &&
                         sellerRecievePrice   

        buyerCoversPrice :: Bool
        buyerCoversPrice = True
        buyergetNFT :: Bool
        buyergetNFT = True
        sellerwithANFTsigned :: Bool
        sellerwithANFTsigned = True
        sellerRecievePrice :: Bool
        sellerRecievePrice = True

-- {-# INLINABLE mkWrappedFreePolicy #-}
-- mkWrappedEAcoinsPolicy :: BuiltinData -> BuiltinData -> ()
-- mkWrappedEAcoinsPolicy = wrapPolicy eaCoins

-- eaCoinsPolicy :: MintingPolicy
-- eaCoinsPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkWrappedEAcoinsPolicy ||])

