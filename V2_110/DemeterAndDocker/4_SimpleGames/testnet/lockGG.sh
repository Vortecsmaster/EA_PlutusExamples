utxoin="9fbddc0a04fdb92f97fe382b955a94308abc75b134bb02baabb879de4226c0bd#0"
address=$(cat guessingGame.addr) 
output="50000000"
PREVIEW="--testnet-magic 2"
# Provide a wallet to see the tx in blockchain explorers
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"


cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-out $address+$output \
  --tx-out-inline-datum-file secretDarjan.json \
  --tx-out $address+$output \
  --tx-out-inline-datum-file secretEddie.json \
  --tx-out $address+$output \
  --tx-out-inline-datum-file secretJim.json \
  --tx-out $address+$output \
  --tx-out-inline-datum-file secretRoberto.json \
  --tx-out $address+"11000000" \
  --tx-out-reference-script-file guessingGame.plutus \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --out-file lockGG.unsigned

cardano-cli transaction sign \
    --tx-body-file lockGG.unsigned \
    --signing-key-file ../../../../../Wallets/Adr01.skey \
    $PREVIEW \
    --out-file lockGG.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file lockGG.signed