utxoin="047378f873909594590f1a27d7818c3c6e4669e7eda16c2998eab93edce807f2#0"
policyid=$(cat EAnftV3.pid)
address=$nami
output="11000000"
tokenamount="-1"
tokenname=$(echo -n "EAnftV3" | xxd -ps | tr -d '\n')
collateral="4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
notneeded="--invalid-hereafter 10962786"

#cardano-cli query protocol-parameters $PREVIEW --out-file protocol.params

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $nami+$output\
  --change-address $Adr01 \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file EAnftV3.plutus \
  --mint-redeemer-file unit.json \
  --protocol-params-file protocol.params \
  --out-file burnTx.body

cardano-cli transaction sign \
    --tx-body-file burnTx.body \
    --signing-key-file Wallet/Adr01.skey \
    --signing-key-file Wallet/Adr07.skey \
    $PREVIEW \
    --out-file burnTx.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file burnTx.signed