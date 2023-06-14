#### Derive address from script
cardano-cli address build --payment-script-file AlwaysSucceedScript.plutus --testnet-magic 2 --out-file AlwaysSucceedScript.addr

#### PubKeyHash creation:
cardano-cli address key-hash --payment-verification-key-file benef1.vkey --out-file benef1.pkh