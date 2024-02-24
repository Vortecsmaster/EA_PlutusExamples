# Create Protocol Parameters
cardano-cli query protocol-parameters --testnet-magic 2 --out-file protocol.params

# Derive PolicyID from Minting Policy Validator
cardano-cli transaction policyid --script-file fileName.plutus > filename.pid