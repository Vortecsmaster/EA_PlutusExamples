utxoin1="3b3c87bc71d72d169af15bf7dd5f1793bc9a40ab6eadcf2d3b3cc66e8ae4de6a#0"
utxoin2="cbe4eb5f4b9ac4be54bb75b2415fd1b0d29f4757e8dd0d86d5260d92c0264118#0"
policyid=$(cat eaNFT.pid)
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"
output="110000000"
tokenamount="1"
tokenname=$(echo -n "EAnft" | xxd -ps | tr -d '\n')
collateral="2b55046a8742d6e149e0c19cd920c691574133341bb695a952a946c45a000d0b#2"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
notneeded="--invalid-hereafter 10962786"
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin2 \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $Adr01+"50000000" \
  --tx-out $Adr01+"60000000" \
  --tx-out $Adr01+$output \
  --tx-out $Adr01+"220000000" \
  --tx-out $Adr01+"50000000" \
  --tx-out $Adr01+"60000000" \
  --tx-out $Adr01+$output \
  --tx-out $Adr01+"220000000" \
  --tx-out $nami+$output+"$tokenamount $policyid.$tokenname" \
  --change-address $Adr01 \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file eaNFT.plutus \
  --mint-redeemer-file Forge.json \
  --protocol-params-file protocol.params \
  --out-file mintEAnftTx.body

cardano-cli transaction sign \
    --tx-body-file mintEAnftTx.body \
    --signing-key-file ../../../../../Wallets/Adr07.skey \
    --signing-key-file ../../../../../Wallets/Adr01.skey \
    $PREVIEW \
    --out-file mintEAnftTx.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file mintEAnftTx.signed