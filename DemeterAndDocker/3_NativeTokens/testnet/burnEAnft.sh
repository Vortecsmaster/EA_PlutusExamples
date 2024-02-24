utxoin1="0172071937c726836d15e477327e2a89376944941f34364a2222913a75fa6a6f#0"
utxoin2="7aa50073a4c112ad6e50d0061dd7764943c95718ad8c0142769f88d9b4a33377#0"
policyid=$(cat eaNFT.pid)
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"
output="110000000"
tokenamount="-1"
tokenname=$(echo -n "EAnft" | xxd -ps | tr -d '\n')
collateral="2b55046a8742d6e149e0c19cd920c691574133341bb695a952a946c45a000d0b#2"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
notneeded="--invalid-hereafter 10962786"
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin1 \
  --tx-in $utxoin2 \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $Adr01+"5000000" \
  --change-address $nami \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file eaNFT.plutus \
  --mint-redeemer-file Burn.json \
  --protocol-params-file protocol.params \
  --out-file burnEAnftTx.body

cardano-cli transaction sign \
    --tx-body-file burnEAnftTx.body \
    --signing-key-file ../../../../../Wallets/Adr07.skey \
    --signing-key-file ../../../../../Wallets/Adr01.skey \
    $PREVIEW \
    --out-file burnEAnftTx.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file burnEAnftTx.signed