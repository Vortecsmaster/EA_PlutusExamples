utxoin="60936c777f8f0549363e15caa4a5365b5e3aadc201193f9dc482c1c848c8a43b#0"
address=$(cat conditionator.addr) 
output="30000000"
collateral="4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0"
signerPKH=$(cat ../../../../Wallets/Adr07.pkh)
nami=$nami
PREVIEW="--testnet-magic 2"

cardano-cli query protocol-parameters --testnet-magic 2 --out-file protocol.params

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 2 \
  --tx-in $utxoin \
  --tx-in-script-file conditionator.plutus \
  --tx-in-datum-file TimeDatum.json \
  --tx-in-redeemer-file redeemTime.json \
  --required-signer-hash $signerPKH \
  --required-signer-hash $(cat ../../../../Wallets/Adr01.pkh) \
  --tx-in-collateral $collateral \
  --tx-out $Adr01+$output \
  --change-address $nami \
  --invalid-hereafter 43455800 \
  --protocol-params-file protocol.params \
  --out-file CCgrab.unsigned

cardano-cli transaction sign \
    --tx-body-file CCgrab.unsigned \
    --signing-key-file ../../../../Wallets/Adr01.skey \
    --signing-key-file ../../../../Wallets/Adr07.skey \
    $PREVIEW \
    --out-file CCgrab.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file CCgrab.signed