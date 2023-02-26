utxoin="0ef2cb8b06d224e898b15518f5bbd7bd134cbb6d4f078203413962c9fb4f7665#0"
address=$(cat MathBounty.addr) 
output="450000000"
collateral="4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-in-script-file MathBounty.plutus \
  --tx-in-datum-file d2.json \
  --tx-in-redeemer-file value12.json \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $Adr01+$output \
  --change-address $nami \
  --invalid-hereafter 10771200 \
  --protocol-params-file protocol.params \
  --out-file grab.unsigned

cardano-cli transaction sign \
    --tx-body-file grab.unsigned \
    --signing-key-file Wallet/Adr07.skey \
    $PREVIEW \
    --out-file grab.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file grab.signed