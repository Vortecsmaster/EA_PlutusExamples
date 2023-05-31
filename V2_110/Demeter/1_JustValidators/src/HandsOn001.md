# HANDS ON 01 - Simple Validators

1. Add a new validator to the AlwaysSucceedandFail module (name at your discretion)
2. Change the logic to the following:
    datum must be equal to redeemer OR
    redeemer must be true (scape condition)
3. Serialize the contract, and the following values:
   1. a number
   2. a boolean
   3. unit 
4. Lock some value on the contract with the following datums
   1. one with datum with a number
   2. one with datum with a boolean
   3. one with datum unit
5. Create the tx to unlock the UTxO with datum boolean
6. Create the tx to unlock the UTxO with datum unit
7. Create the tx to unlock the UTxO with datum number