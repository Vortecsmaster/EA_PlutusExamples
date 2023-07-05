#### Derive address from script
cardano-cli address build --payment-script-file mathBounty.plutus --testnet-magic 2 --out-file mathBounty.addr

#### PubKeyHash creation:
cardano-cli address key-hash --payment-verification-key-file benef1.vkey --out-file benef1.pkh

#### Create Procolo parameters file
cardano-cli query protocol-parameters --testnet-magic 2 --out-file protocol.params