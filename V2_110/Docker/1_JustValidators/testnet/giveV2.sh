utxoin="da14eebc1c7c343e1cdb4d9a64733b4c62cf7ae143434bec7339929a9ef431c6#0"
address=$(cat AS.addr) 
output="110000000"
address2=$(cat DvR.addr)
output2="8000000"


cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-out $address2+$output2 \
  --tx-out-reference-script-file DvR.plutus \
  --tx-out-inline-datum-file unit.json \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --out-file give.unsigned

cardano-cli transaction sign \
    --tx-body-file give.unsigned \
    --signing-key-file Wallet/Adr01.skey \
    $PREVIEW \
    --out-file give.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file give.signed