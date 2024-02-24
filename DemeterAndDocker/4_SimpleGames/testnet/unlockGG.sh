utxoin=""
output=""
collateral="f2e87ceaefb5810f934c527d93e1c8cec440968c49a362682aa90e0e96db54ff#2"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt" 
PREVIEW="--testnet-magic 2"
RefScript="cd79128d6be3f1f6e79b93e2f3f8fe99a87d1d7537355c483e5b6a91df88b7f6#4"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --spending-tx-in-reference $RefScript \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file guessRoberto.json \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $Adr01+$output \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --out-file unlockGG.unsigned 

cardano-cli transaction sign \
    --tx-body-file unlockGG.unsigned \
    --signing-key-file ../../../../../Wallets/Adr07.skey \
    $PREVIEW \
    --out-file unlockGG.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file unlockGG.signed