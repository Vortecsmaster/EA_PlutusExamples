cardano-cli transaction build \
  --alonzo-era \
  $TESTNET \
  --tx-in 4a5d4091600a8ea3eac1c3cde32d99c4514541888b071007a69ccdcf6dfab639#1 \
  --tx-out $(cat vesting.addr)+5000000 \
  --tx-out-datum-hash-file unit.json \
  --change-address $nami \
  --out-file tx.unsigned

cardano-cli transaction sign  \
  --tx-body-file tx.unsigned  \
  --signing-key-file Adr07.skey  \
  $TESTNET  \
  --out-file tx.signed

cardano-cli transaction submit  \
  $TESTNET \
  --tx-file tx.signed