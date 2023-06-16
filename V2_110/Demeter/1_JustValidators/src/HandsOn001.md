# HANDS ON 01 - Simple Validators

## HANDSON No. 01:  untyped Datum vs Redeemer

1. Add a new validator to the AlwaysSucceedandFail module (name at your discretion)
2. Change the logic to the following:
    datum must be equal to redeemer OR
    redeemer must be 11 (escape condition)
3. Serialize the contract, and the following values:
   1. a number
   2. a boolean
   3. unit 
4. Lock some value on the contract with the following datums
   1. one with datum with a number
   2. one with datum with a boolean
   3. one with datum unit
5. Unlock the value from the UTxOs created on previous step 4
   1. the UTxO with a datum with a number
   2. the UTxO with a datum with a boolean
   3. the UTxO with a datum unit