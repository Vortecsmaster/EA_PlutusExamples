# TestNet Execution - Mathbounty contract

Summary:
> This exercise is going to walkthrough in executing a smart contract on a testnet, by serializing the different requirements for the transaction construction and finally constructing and submitting your transaction for execution using Cardano-CLI.


## Part 1 Prepare the elements necessary Transaction construction 

What do we need?

* A working Plutus Developer Environment Nix-Shell
* 1 paymentAddresses with some UTxO with ADA for providing Bounty Value.
* 1 paymentAddress with a UTxO with ADA **ONLY** for providing collateral. (Could be the same payment address)
* Validator Script (Serialized and JSON encoded)
* Datum (JSON encoded)
* Redeemer (JSON encoded)


## Part 2 Serializing and encoding 

###### STEP 1
    Run your NIX-SHELL

###### STEP 2
In the your MathBountyTestnet folder execute 

    cabal repl

###### STEP 3
On the REPL, Evaluate the functions on the Deploy module

    writeUnit
    writeDatum
    writeRedeemer
    writeBadRedeemer
    writeBountyValidator

This is going to create the corresponding encoded/serialized files for unit, datum, goodRedeeer, badRedeemer and mathBounty.plutus

###### STEP 3


 **Fund the payment address with 10000 Test Ada from the Faucet (https://testnets.cardano.org/en/testnets/cardano/tools/faucet/)**

