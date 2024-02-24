# HANDS ON 03 - Common Conditions for Validtoars

## HANDSON No. 03:  All or nothing!

1. Create a new module with a  new validator to Validate_All_or_Nothing  (name at your discretion)
2. Change the logic to the following:
   * User signature AND
   * Tx execution BEFORE timeline AND
   * Value provided must be *at least* 2 times the price.
3. Serialize the contract, and the following values:
   1. datum with your selected wallet pubkeyhash, timeline and price. (you select them, this are arbitrary conditions)
   2. Feel free to create several with different values.   
4. Lock some value on the contract with the previous datums. 
5. Unlock the value from the UTxOs created on previous step, on a single transaction.
   1. all the UTxO that can be validated for a specific set of conditions.

Observations:
    Notice that different datums might validate the same conditions.