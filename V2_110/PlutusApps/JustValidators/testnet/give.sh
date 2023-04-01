utxoin="a6800bc5e30bfc555a39425b68c15d052c99310867099eda32ef14f915558ae8#0"
address=$(cat AS.addr) 
output="100000000"


cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-out $address+$output \
  --tx-out-datum-hash-file unit.json \
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