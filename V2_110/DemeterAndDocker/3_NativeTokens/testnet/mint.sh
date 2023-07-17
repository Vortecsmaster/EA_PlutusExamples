utxoin="0172071937c726836d15e477327e2a89376944941f34364a2222913a75fa6a6f#1"
policyid=$(cat testCoin.pid)
nami="addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"
output="11000000"
tokenamount="11"
tokenname=$(echo -n "testcoins" | xxd -ps | tr -d '\n')
collateral="2b55046a8742d6e149e0c19cd920c691574133341bb695a952a946c45a000d0b#2"
signerPKH="697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55"
signer2PKH="8fd2af318fe6fd7a8b2f56861b7dda312411281616b902953abf7121"
notneeded="--invalid-hereafter 10962786"
PREVIEW="--testnet-magic 2"

cardano-cli transaction build \
  --babbage-era \
  $PREVIEW \
  --tx-in $utxoin \
  --required-signer-hash $signerPKH \
  --required-signer-hash $signer2PKH \
  --tx-in-collateral $collateral \
  --tx-out $nami+$output+"$tokenamount $policyid.$tokenname" \
  --change-address $nami \
  --mint "$tokenamount $policyid.$tokenname" \
  --mint-script-file handson07.plutus \
  --mint-redeemer-file unit.json \
  --protocol-params-file protocol.params \
  --out-file mintTx.body

cardano-cli transaction sign \
    --tx-body-file mintTx.body \
    --signing-key-file ../../../../../Wallets/Adr01.skey \
    --signing-key-file ../../../../../Wallets/Adr07.skey \
    $PREVIEW \
    --out-file mintTx.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file mintTx.signed