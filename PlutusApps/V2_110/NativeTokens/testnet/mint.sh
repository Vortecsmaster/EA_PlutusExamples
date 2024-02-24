utxoin="8dfbc683114a903264cfe7b66c7b2935f3afbbfdcfabda3ba0fe2ecb80869d07#0"
policyid=$(cat EAnftV3.pid)
address=$nami
output="11000000"
tokenamount="1"
tokenname=$(echo -n "EAnftV3" | xxd -ps | tr -d '\n')
collateral="4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
notneeded="--invalid-hereafter 10962786"


cardano-cli query protocol-parameters $PREVIEW --out-file protocol.params

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $nami+$output+"$tokenamount $policyid.$tokenname" \
  --change-address $Adr01 \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file EAnftV3.plutus \
  --mint-redeemer-file True.json \
  --metadata-json-file metadata.json \
  --protocol-params-file protocol.params \
  --out-file mintTx.body

cardano-cli transaction sign \
    --tx-body-file mintTx.body \
    --signing-key-file Wallet/Adr01.skey \
    --signing-key-file Wallet/Adr07.skey \
    $PREVIEW \
    --out-file mintTx.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file mintTx.signed