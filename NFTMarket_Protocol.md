#  Example Usecase -> NFT Marketplace dApp

## Actions Outline

List NFT
Buy NFT (Taker and Maker)
UpdatePrice NFT
CancelListing NFT


Borrow NFT
Lend NFT
PayBack NFT
ClawBack NFT

## Script Outline

Sell Script

Lend Script

(MadeLoad -> Lend Script)
MadeLoan Script

## Transaction Outline

### TX 1 (List NFT)

In:
User1 (ADA + NFT)

Out:
Sell Script [ owner: Address(key, Maybe Key)], recipient: Address,  (NFT)

### TX 2 (Buy NFT [Taker])

In:
User2 (X ADA)
ref1 @ Sell Script [...recipient: Address] (NFT)
ref2 Sell Script [...recipient: Address] (NFT2)  -> Will fail <- Due to solution of double satisfaction>


Out:
User2 (NFT + NFT2)
recipient [ ref1 ] (X ADA)

### TX 3 (Lend NFT)

In:
User1 (ADA + NFT)

Out:
Lend  Script [ owner: Address, borrower: Address, collateral: Value, interest: Value, duration: Integer, nft: CurrencySymbol] (NFT)

### TX 4 (Borrow NFT)

In:
Lend Script [ ... ] (NFT)
User4 (X ADA)

Out:
User4 (NFT)
LendNFT  [... borrower-User4, ] 
PaybackNFT( ... borrower ->  

## Script Logic  Outline  *Outline the logic of Sell NFT

### Sell Script

Datum:
owner: Address
recipient: Address
price: Value

Redeemer:
Buy NFT

Buy NFT:
= recipient gest price (datum = utxoRef of Self)

### Lend Script

Datum:
owner: Address
collateral: Value
interest: Value
duration: Integer
nft: CurrencySymbol 

Redeemer:
Cancel 
Borrow
Update

### MadeLoan Script

Datum:
owner: Address
borrower: Address
interest: Value
start: Integer
duration: Integer
nft: CurrencySymbol

Redeemer:
PayBack
ClawBack 

## Potential Vulnerabilities
    
Buy Script Doble satisfaction