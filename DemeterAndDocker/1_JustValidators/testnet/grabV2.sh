utxoin1="b98779b7d5bf2d8aea61a5631567a4bab31547f7094b16972755d58278fd98e7#0"
utxoin2="d01b822301349f8430d7f6fa572c0308013bfcea480af5326471e649a55adfc8#1"
utxoin3="d01b822301349f8430d7f6fa572c0308013bfcea480af5326471e649a55adfc8#2"

refscript="d01b822301349f8430d7f6fa572c0308013bfcea480af5326471e649a55adfc8#3"

address=$Adr01
output="140000000"
collateral="4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin1 \
  --spending-tx-in-reference $refscript \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file value22.json \
  --tx-in $utxoin2 \
  --spending-tx-in-reference $refscript \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file value11.json \
  --tx-in $utxoin3 \
  --spending-tx-in-reference $refscript \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file True.json \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $address+$output \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --out-file grabV2.unsigned

cardano-cli transaction sign \
    --tx-body-file grabV2.unsigned \
    --signing-key-file ../../../../Wallets/Adr07.skey \
    $PREVIEW \
    --out-file grabV2.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file grabV2.signed