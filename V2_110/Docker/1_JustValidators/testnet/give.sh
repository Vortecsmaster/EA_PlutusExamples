utxoin="0064391f48e814a5259e905f426e438c65ffba6b0bb750c7a438f58c35370c23#0"
address=$(cat datum22.addr) 
output="500000000"
PREVIEW="--testnet-magic 2"
nami="<provide a wallet to see the tx in blockchain explorers>" 


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
    --signing-key-file ialice.skey \
    $PREVIEW \
    --out-file give.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file give.signed