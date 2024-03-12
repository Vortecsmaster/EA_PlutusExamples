cardano-cli address build --payment-script-file alwaysSucceeds.plutus --testnet-magic 2 --out-file alwaysSucceeds.addr
cardano-cli address build --payment-script-file alwaysFails.plutus --testnet-magic 2 --out-file alwaysFails.addr
cardano-cli address build --payment-script-file redeemer11.plutus --testnet-magic 2 --out-file redeemer11.addr
cardano-cli address build --payment-script-file datum22.plutus --testnet-magic 2 --out-file datum22.addr
cardano-cli address build --payment-script-file datum23.plutus --testnet-magic 2 --out-file datum23.addr