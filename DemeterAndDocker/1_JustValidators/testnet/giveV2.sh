utxoin="0172071937c726836d15e477327e2a89376944941f34364a2222913a75fa6a6f#7"
address=$(cat dEr.addr) 
output="70000000"
address2=$(cat alwaysFails.addr)
output2="8000000"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 2 \
  --tx-in $utxoin \
  --tx-out $address+$output \
  --tx-out-inline-datum-file value22.json \
  --tx-out $address+$output \
  --tx-out-inline-datum-file unit.json \
  --tx-out $address+$output \
  --tx-out-inline-datum-file True.json \
  --tx-out $address2+$output2 \
  --tx-out-reference-script-file dEr.uplc \
  --tx-out-inline-datum-file unit.json \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --out-file give.unsigned

cardano-cli transaction sign \
    --tx-body-file give.unsigned \
    --signing-key-file ../../../../Wallets/Adr01.skey \
    --testnet-magic 2 \
    --out-file give.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file give.signed