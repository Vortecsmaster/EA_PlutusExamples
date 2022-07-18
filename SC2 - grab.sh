cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 1097911063 \
  --tx-in fa3af564fff04bd5b77a9a39f0e6055acf7eeb7358516754855b30fcbb4524d5#1 \
  --tx-in-script-file ./vesting.plutus \
  --tx-in-datum-file ./unit.json  \
  --tx-in-redeemer-file ./unit.json \
  --required-signer-hash 1dcdf420c1488ba345730d41f72a846428ba814c2bd639462eaf5a07 \
  --tx-in-collateral 67557dbe63d46e252276c729ebf75afe615c9903b644a37c11f6f0ac22fa8aff#0 \
  --change-address $nami \
  --invalid-before 61967066 \
  --protocol-params-file ./protocol.params \
  --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file payment2.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed