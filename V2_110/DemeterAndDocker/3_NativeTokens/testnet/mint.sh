utxoin="120eb8cc3185a55cda7af4a8dbe06a64be83680fa7bca79d25a8a4f845e05448#0"
policyid=$(cat EAcoins.pid)
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"
output="110000000"
tokenamount="101"
tokenname=$(echo -n "EAcoins" | xxd -ps | tr -d '\n')
collateral="64819d9d96cb1a71d7da335cfbbf8da5fe681ef6e2a260f1423cf6e8cf2dadbb#0"
signerPKH="aabd57f38bb62852e9da6c8822aec1f24ef30d917b834ecfd6668c98"
notneeded="--invalid-hereafter 10962786"
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $nami+$output+"$tokenamount $policyid.$tokenname" \
  --change-address $(cat ialice.addr) \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file EAcoins.plutus \
  --mint-redeemer-file deadline.json \
  --invalid-hereafter 20710503 \
  --protocol-params-file protocol.params \
  --out-file mintTx.body

cardano-cli transaction sign \
    --tx-body-file mintTx.body \
    --signing-key-file ialice.skey \
    $PREVIEW \
    --out-file mintTx.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file mintTx.signed