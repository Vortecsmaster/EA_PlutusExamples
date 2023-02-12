utxoin="612f766282d47b091e3c7372405a3728d752e918ea79251db456d367bbac5ddb#0"
funds="25000000"
policyid=$(cat policy.id)
address=$(cat Wallet/Adr01.addr)
output="5000000"
tokenamount="1"
tokenname=$(echo -n "myWonderfullNFT" | xxd -ps | tr -d '\n')
collateral="4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $address+$output+"$tokenamount $policyid.$tokenname" \
  --change-address $Adr01 \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file nftMintV2.plutus \
  --mint-redeemer-file unit.json \
  --invalid-hereafter 3371469 \
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