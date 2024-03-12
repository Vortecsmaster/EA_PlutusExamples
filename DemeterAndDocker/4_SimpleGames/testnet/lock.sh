utxoin="0172071937c726836d15e477327e2a89376944941f34364a2222913a75fa6a6f#3"
address=$(cat mathBounty.addr) 
output=""
PREVIEW="--testnet-magic 2"
# Provide a wallet to see the tx in blockchain explorers
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"


cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-out $address+$output \
  --tx-out-datum-hash-file bountyConditions.json \
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