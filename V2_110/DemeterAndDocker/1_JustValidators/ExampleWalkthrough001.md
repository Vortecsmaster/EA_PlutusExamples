## EXAMPLE 01: Simple Validators

_Navigate to the folder 1_JustValidators_

#### Lock and unlock some value on the scriptAddress of the redeemer11.plutus validator.

###### Requires:  At least one funded payment address with its correspoding keys. 
```Bash
cardano-cli address key-gen \
  --verification-key-file payment1.vkey \
  --signing-key-file payment1.skey

cardano-cli stake-address key-gen \
  --verification-key-file stake1.vkey \
  --signing-key-file stake1.skey

cardano-cli address build \
--payment-verification-key-file payment1.vkey \
--stake-verification-key-file stake1.vkey \
--out-file payment1.addr \
--testnet-magic 2
```
###### Use the [Cardano Testnets faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/) to provide funds for your testing
---
###### STEP 1 - Serialize the On-Chain Validator
1.1. Enter the REPL
```Bash
cabal repl
```
1.2. Evaluate function redeemer11
```Haskell
Prelude AlwaysSucceedandFail> saveRedeemer11
```
_Get the message_
```Haskell
Serialized script to: ./testnet/redeemer11.plutus
```
_The content must look like this:_

```JSON
{
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "581e581c0100002225335333573466ebc008dd42402c2440042440022240022d"
}
```
1.3. Serialize the values necesary
```Haskell
Prelude AlwaysSucceedandFail> saveValue11

```
_Get the message_
```Haskell
Wrote data to: ./testnet/value11.json
{
    "int": 11
}
```
_The content must look like this:_
```JSON
{"int":11}
```
---
###### STEP 2 - Derive the scriptAddress form the validator script
```Bash
cardano-cli address build  \
  --payment-script-file redeemer11.plutus  \
  --testnet-magic 2 \
  --out-file redeemer11.addr
```
_redeemer11.addr must be_
```JSON
addr_test1wrrdjclg3y53d2u820fuxssr0ngjyy3ufhtc8gr67g0cmtqv0r6ez
```
###### STEP 3 - Lock value on the scriptAddress
3.1. Edit the give script (give.sh) and update the variables
* **utxoin** = Select a UTXO (TxHash + Index)
* **address** = $(cat redeemer11.addr)
* **output** = Any ammount lower than the value on the UTxO, leaving enough to cover fees and some change.
* **nami** = provided your webWallet address, if you want to send the change.
* Select any Datum value of your preference in line 13.
  Example:   _--tx-out-datum-hash-file **unit.json**_ \ 

3.2. You must be able to execute this script
```Bash
chmod +x give.sh
```

3.3. Execute the script
```Bash
./give.sh
```

3.4. Verify value on the contract 
```Bash
cardano-cli query utxo \
  --address $(cat redeemer11.addr) \
  --testnet-magic 2
```
_or use your webwallet history or the redeemer11 validator scriptAddress to see on explorer._

###### STEP 4 - Unlock the value on the scriptAddress
4.1. Edit the give script (grab.sh) and update the variables
* **utxoin** = Select a UTXO (TxHash + Index)
* **address** = The account address that you send the value to.
* **output** = Any ammount lower than the value on the UTxO, leaving enough to cover fees and some change.
* **collateral** = Select an ADA only UTxO to pay fees in case of on-chain failure (phase 2 validation).
* **signerPKH** = The provider of the collateral UTxO verification key hash (also known as pubKeyHash). 
* **nami** = provided your webWallet address, if you want to send the change.
* Submit any Redeemer value that fullfill the validator logic in line 15.
  Example:   _--tx-in-redeemer-file **value11.json**_ \   

4.2. Derive the verification key-hash
```Bash
cardano-cli address key-hash --payment-verification-key-file payment1.vkey --out-file payment1.pkh
```

4.3. You must be able to execute this script
```Bash
chmod +x grab.sh
```
4.4. Execute the script
```Bash
./grab.sh
```
_verify that the value is taken from the contract_

