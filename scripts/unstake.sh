#!/bin/bash

CS="b090c134738121e6c96dd9eedc3f7a99e6ac9d21985c1b97cba72077"
TN="b9df48c3f4614337d7c67fc4ecd81e404cc75b89ef348a92e6d34f02a70b242e"

SCRIPT_UTXO="20669222de6e9daf540312c98ac7779ed6ad573af6319fed637039420709a90c#0"
COLLATERAL_UTXO="33e8497eca83e1efbe3c1383da7ead3961fcdb3bd631c70efe6fb80b189d9e4d#0"

TX_IN1="20669222de6e9daf540312c98ac7779ed6ad573af6319fed637039420709a90c#1"
RO_REF="837736493d53e82fc506de9272d9bc7af4035b49996ff81b6e2c6921b2a5f184#0"

STAKING_CONTRACT_ADDRESS="addr_test1wqkax9glhwhss8e5x3dvfcgy9m3hy26yr6srwxkct0755mgd3xcr6"
STAKING_TOKEN="3f1d9bd2f8c3d7d8144b789433261370eaf25cdc83fe8a745ef880c1.744452415341"

CHANGE_ADDRESS="addr_test1qqt86eq9972q3qttj6ztje97llasktzfzvhmdccqjlqjaq2cer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qy6q5t2"
CHANGE="11175960 + 4990 $STAKING_TOKEN"

# Build Staking Transaction
cardano-cli transaction build \
--babbage-era \
--tx-in $SCRIPT_UTXO \
--tx-in-script-file "../wmt_staking_dummy.plutus" \
--tx-in-inline-datum-present \
--tx-in-redeemer-file "./unstake_redeemer_val.json" \
--tx-in $COLLATERAL_UTXO \
--tx-in $TX_IN1 \
--read-only-tx-in-reference "837736493d53e82fc506de9272d9bc7af4035b49996ff81b6e2c6921b2a5f184#0" \
--change-address "addr_test1qqt86eq9972q3qttj6ztje97llasktzfzvhmdccqjlqjaq2cer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qy6q5t2" \
--mint "-1 $CS.$TN" \
--mint-script-file "../wmt-staking-proof-of-execution_dummy.plutus" \
--mint-redeemer-file "./unstake_redeemer_pol.json" \
--tx-in-collateral $COLLATERAL_UTXO \
--required-signer-hash "167d64052f9408816b9684b964befffb0b2c49132fb6e30097c12e81" \
--testnet-magic 2 \
--out-file "./unstake_unsigned_build.tx"

cardano-cli transaction build-raw \
--babbage-era \
--tx-in $SCRIPT_UTXO \
--tx-in-script-file "../wmt_staking_dummy.plutus" \
--tx-in-inline-datum-present \
--tx-in-redeemer-file "./unstake_redeemer_val.json" \
--tx-in-execution-units "(1055772774, 3928168)" \
--tx-in $COLLATERAL_UTXO \
--tx-in $TX_IN1 \
--read-only-tx-in-reference "837736493d53e82fc506de9272d9bc7af4035b49996ff81b6e2c6921b2a5f184#0" \
--tx-out "$CHANGE_ADDRESS + $CHANGE" \
--mint "-1 $CS.$TN" \
--mint-script-file "../wmt-staking-proof-of-execution_dummy.plutus" \
--mint-redeemer-file "./unstake_redeemer_pol.json" \
--mint-execution-units "(1055772774, 3928168)" \
--tx-in-collateral $COLLATERAL_UTXO \
--required-signer-hash "167d64052f9408816b9684b964befffb0b2c49132fb6e30097c12e81" \
--protocol-params-file "./protocolparams.json" \
--fee 1200000 \
--out-file "./unstake_unsigned_build_raw.tx"

# --mint-execution-units "(1055772774, 3928168)" \
# --fee 700000 \
# --tx-out "addr_test1qqt86eq9972q3qttj6ztje97llasktzfzvhmdccqjlqjaq2cer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qy6q5t2 + 11600000 + 5000 3f1d9bd2f8c3d7d8144b789433261370eaf25cdc83fe8a745ef880c1.744452415341" \

# Sign with eternl
# Create a Witness file
# Use the existing witness file and replace the signature, attention:!!! You need to erase the first 6 bytes, eternl has wrappen the signature as TxWitness !!! 
# This witness has correct length as comparsion:
# 825820fd8b0d639478778be68d760f1169e69264a29a2e19e9aec4be02f00d4db3baaf5840e75788d6d253495e5e2989d46f341db34ee87c629995a7a043bc6bf0566aba373a6fe201c2508cdf27f2ccc70eda5f864f6e3b5cd178bb35a33428836842be0a

# Then Assemble tx body and witness
# cardano-cli transaction assemble --tx-body-file unstake_unsigned_build_raw.tx --witness-file witness.tx --out-file signed.tx

# Then Submit to network
# cardano-cli transaction submit --tx-file signed.tx --testnet-magic 2

# Get TxHash
# cardano-cli transaction txid --tx-file signed.tx