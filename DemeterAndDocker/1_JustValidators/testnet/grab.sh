utxoin="b6969f003401ab3e242a4e8db7e903ac5131b3a028a80749b519bf142c720ab0#0"
address=$(cat ialice.addr) 
output="450000000"
collateral="4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0"
signerPKH=$(cat ialice.pkh)
nami="<provide a wallet to see the tx in blockchain explorers>" 
PREVIEW="--testnet-magic 2"

cardano-cli query protocol-parameters --testnet-magic 2 --out-file protocol.params

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 2 \
  --tx-in e649789f360708ba1b58b52ac9af167900055c846294a4bdae1fc11447301ba8#0 \
  --tx-in-script-file datum23.plutus \
  --tx-in-datum-file value23.json \
  --tx-in-redeemer-file unit.json \
  --required-signer-hash 697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55 \
  --tx-in-collateral 4cbf990857530696a12b0062546a4b123ad0bef21c67562e32d03e3288bdcd7b#0 \
  --tx-out "addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt 5000000" \
  --change-address addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt \
  --protocol-params-file protocol.params \
  --out-file grab.unsigned

cardano-cli transaction sign \
    --tx-body-file grab.unsigned \
    --signing-key-file ../../../../Wallets/Adr07.skey \
    $PREVIEW \
    --out-file grab.signed

 cardano-cli transaction submit \
    $PREVIEW \
    --tx-file grab.signed