# HANDS ON 02 - Typed Validators

## HANDSON No. 02:  custom typed Datum vs Redeemer

1. Add a new validator to the TypedValidators module (name at your discretion)
2. Change the logic to the following:
    custom typed datum must be equal to redeemer when custom type as a wrapped integer OR
    a wrapped boolean.
3. Serialize the contract, and the following values:
   1. wrapped 33
   2. wrapped False
   3. wrapped True 
4. Lock some value on the contract with the following datums
   1. datum with unit
   2. datum wrapped 33
   3. datum wrapped True
   4. datum wrapped False
5. Unlock the value from the UTxOs created on previous step 4, on a single transaction.
   1. all the UTxO that can be validated