utxoin="3b3c87bc71d72d169af15bf7dd5f1793bc9a40ab6eadcf2d3b3cc66e8ae4de6a#0"
policyid=$(cat 1signMP.pid)
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"
output="1100000000"
tokenamount="1001"
tokenname=$(echo -n "WhateverCoin" | xxd -ps | tr -d '\n')
collateral="2b55046a8742d6e149e0c19cd920c691574133341bb695a952a946c45a000d0b#2"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
notneeded="--invalid-hereafter 10962786"
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $Adr01+11000000 \
  --tx-out $nami+$output+"$tokenamount $policyid.$tokenname" \
  --change-address $nami \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file 1signMP.plutus \
  --mint-redeemer-file unit.json \
  --protocol-params-file protocol.params \
  --out-file mintTx.body

cardano-cli transaction sign \
    --tx-body-file mintTx.body \
    --signing-key-file $WALLETS/Adr01.skey \
    --signing-key-file $WALLETS/Adr07.skey \
    $PREVIEW \
    --out-file mintTx.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file mintTx.signed