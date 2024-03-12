utxoin="0172071937c726836d15e477327e2a89376944941f34364a2222913a75fa6a6f#6"
address=$(cat conditionator.addr) 
output="50000000"
PREVIEW="--testnet-magic 2"
nami=$nami 

cardano-cli query protocol-parameters --testnet-magic 2 --out-file protocol.params

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-out $address+$output \
  --tx-out-datum-hash-file TimeDatum.json \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --out-file give.unsigned

cardano-cli transaction sign \
    --tx-body-file give.unsigned \
    --signing-key-file ../../../../Wallets/Adr01.skey \
    $PREVIEW \
    --out-file give.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file give.signed