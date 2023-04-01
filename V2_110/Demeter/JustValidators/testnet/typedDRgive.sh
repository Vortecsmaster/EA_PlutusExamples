utxoin="38956583c3fb800ffbaa8e8d6b2adfb26a19806ca72cf47d10bd1b72223ecb04#1"
address=$(cat typedDvR.addr) 
output="1000000000"


cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-out $address+$output \
  --tx-out-datum-hash-file td19.json \
  --change-address $Adr01 \
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