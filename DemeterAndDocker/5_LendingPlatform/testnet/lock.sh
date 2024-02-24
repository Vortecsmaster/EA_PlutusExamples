utxoin="4b6bda5d37598571a89dc876885c5a973bb50e405a50a8545c6f65d395176739#5"
address=$(cat mathBounty.addr) 
output="50000000"
PREVIEW="--testnet-magic 2"
# Provide a wallet to see the tx in blockchain explorers
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"



cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-out $address+$output \
  --tx-out-datum-hash-file bountyConditions17.json \
  --tx-out $address+$output \
  --tx-out-datum-hash-file bountyConditions18.json \
  --tx-out $address+$output \
  --tx-out-datum-hash-file bountyConditions19.json \
  --tx-out $address+$output \
  --tx-out-datum-hash-file bountyConditions20.json \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --out-file lock.unsigned

cardano-cli transaction sign \
    --tx-body-file lock.unsigned \
    --signing-key-file ../../../../../Wallets/Adr01.skey \
    $PREVIEW \
    --out-file lock.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file lock.signed