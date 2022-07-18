Cardano CLI commands

PubKeyHash creation:

cardano-cli address key-hash --payment-verification-key-file Adr07.vkey --out-file Adr07.pkhb



Script address creation: (on-chain validator publication?)

cardano-cli address build --script-file vesting.plutus $TESTNET --out-file vesting.addr



Protocol Parameters to file

cardano-cli query protocol-parameters $TESTNET --out-file protocol.params

