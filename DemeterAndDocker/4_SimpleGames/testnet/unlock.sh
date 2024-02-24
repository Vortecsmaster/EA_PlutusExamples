utxoin="33d892c3e103535f99ed8201942684601339899fee456f1f1a8e62f1e199c38f#0"
output="25000000"
collateral="f2e87ceaefb5810f934c527d93e1c8cec440968c49a362682aa90e0e96db54ff#2"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt" 
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-in-script-file guessingGame.plutus \
  --tx-in-datum-file secret.json \
  --tx-in-redeemer-file guess.json \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $Adr02+$output \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --out-file unlock.unsigned 
 #--invalid-hereafter 21914085 \

cardano-cli transaction sign \
    --tx-body-file unlock.unsigned \
    --signing-key-file ../../../../../Wallets/Adr07.skey \
    $PREVIEW \
    --out-file unlock.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file unlock.signed