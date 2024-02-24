# HANDS ON 04 - Multisignature

## HANDSON No. 04:  Think and design a multisignature contract

1. Create a new module with a  new validator to Multisignarure  (name at your discretion)
2. Create a DATUM type that contains a list of pubkeyhashes.
3. Change the logic to the following:
   * PubKeyHashes on the datum must be in the Signatories of the ScriptContext
4. Serialize the contract, and the following values:
   1. datum with your selected wallets pubkeyhashes.
 5. Lock some value on the contract with the previous datum. 
6. Unlock the value from the UTxOs created on previous step, on a single transaction.
   1. Consider the eUTxO model of this Transactino definition.

Observations:
    Notice the design of the transaction aligning with the on-chain validator requirements.
    