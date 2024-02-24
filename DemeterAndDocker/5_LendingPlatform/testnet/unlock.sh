utxoin="2468200e234dab20f4610aa468b82fd04c581f53189d2d650ebeeee93fe6eccc#3"
output="40000000"
collateral="2b55046a8742d6e149e0c19cd920c691574133341bb695a952a946c45a000d0b#2"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt" 
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --tx-in-script-file mathBounty.plutus \
  --tx-in-datum-file bountyConditions20.json \
  --tx-in-redeemer-file value5.json \
  --required-signer-hash $signerPKH \
  --tx-in-collateral $collateral \
  --tx-out $Adr01+$output \
  --change-address $nami \
  --protocol-params-file protocol.params \
  --invalid-hereafter 21914085 \
  --out-file unlock.unsigned 

cardano-cli transaction sign \
    --tx-body-file unlock.unsigned \
    --signing-key-file ../../../../../Wallets/Adr07.skey \
    $PREVIEW \
    --out-file unlock.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file unlock.signed