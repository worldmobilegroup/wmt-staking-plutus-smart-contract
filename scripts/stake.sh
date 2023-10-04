#!/bin/bash

CS="b090c134738121e6c96dd9eedc3f7a99e6ac9d21985c1b97cba72077"
TN="b9df48c3f4614337d7c67fc4ecd81e404cc75b89ef348a92e6d34f02a70b242e"

TX_IN1="b9df48c3f4614337d7c67fc4ecd81e404cc75b89ef348a92e6d34f02a70b242e#0"
RO_REF="837736493d53e82fc506de9272d9bc7af4035b49996ff81b6e2c6921b2a5f184#0"

STAKING_CONTRACT_ADDRESS="addr_test1wqkax9glhwhss8e5x3dvfcgy9m3hy26yr6srwxkct0755mgd3xcr6"
STAKING_TOKEN="3f1d9bd2f8c3d7d8144b789433261370eaf25cdc83fe8a745ef880c1.744452415341"

CHANGE_ADDRESS="addr_test1qqt86eq9972q3qttj6ztje97llasktzfzvhmdccqjlqjaq2cer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qy6q5t2"
CHANGE="5375960 + 4985 3f1d9bd2f8c3d7d8144b789433261370eaf25cdc83fe8a745ef880c1.744452415341"
COLLATERAL_UTXO="33e8497eca83e1efbe3c1383da7ead3961fcdb3bd631c70efe6fb80b189d9e4d#0"
REQ_PKH="167d64052f9408816b9684b964befffb0b2c49132fb6e30097c12e81"

# Build Staking Transaction
cardano-cli transaction build \
--babbage-era \
--tx-in $TX_IN1 \
--read-only-tx-in-reference $RO_REF \
--tx-out "$STAKING_CONTRACT_ADDRESS + 2000000 + 5 $STAKING_TOKEN  + 1 $CS.$TN" \
--tx-out-inline-datum-file "./stake_datum.json" \
--change-address $CHANGE_ADDRESS \
--mint "1 $CS.$TN" \
--mint-script-file "../wmt-staking-proof-of-execution_dummy.plutus" \
--mint-redeemer-file "./stake_redeemer.json" \
--required-signer-hash $REQ_PKH \
--tx-in-collateral $COLLATERAL_UTXO \
--protocol-params-file "./protocolparams.json" \
--testnet-magic 2 \
--out-file "./stake_unsigned_build.tx"


cardano-cli transaction build-raw \
--babbage-era \
--tx-in $TX_IN1 \
--read-only-tx-in-reference $RO_REF \
--tx-out "$STAKING_CONTRACT_ADDRESS + 2000000 + 5 $STAKING_TOKEN  + 1 $CS.$TN" \
--tx-out-inline-datum-file "./stake_datum.json" \
--tx-out "$CHANGE_ADDRESS + $CHANGE" \
--mint "1 $CS.$TN" \
--mint-script-file "../wmt-staking-proof-of-execution_dummy.plutus" \
--mint-redeemer-file "./stake_redeemer.json" \
--mint-execution-units "(1055772774, 3928168)" \
--required-signer-hash $REQ_PKH \
--tx-in-collateral $COLLATERAL_UTXO \
--protocol-params-file "./protocolparams.json" \
--fee 800000 \
--out-file "./stake_unsigned_build_raw.tx"


# Preview-Testnet Parameters first version
# cabal run wmt-staking d8bebcb0abd89193874c59ed3023f5b4f81b89b6676d187ad7fbdb0e 3f1d9bd2f8c3d7d8144b789433261370eaf25cdc83fe8a745ef880c1 tDRASA addr_test1wzffrh2eu39sfrmty89zma353yulrjtnphx533xz8985jpsdfa2t3 addr_test1qz43jnthgl2qk97fvkqwg9affwnfn7v2j8jycn7q6m6d90wp5n9fnvekx7cv5kye9k5xwlrqgylxlu4hdc7d85mhu6yq7760j3 2
# Smart Contract Generation Output
# {"plutus_file_policy":"wmt-staking-proof-of-execution_dummy.plutus","currencysymbol":"b090c134738121e6c96dd9eedc3f7a99e6ac9d21985c1b97cba72077","plutus_file_validator":"wmt_staking_dummy.plutus","validator_address":"addr_test1wqkax9glhwhss8e5x3dvfcgy9m3hy26yr6srwxkct0755mgd3xcr6"}
# PubKeyHash: 167d64052f9408816b9684b964befffb0b2c49132fb6e30097c12e81 
# Bytes nftcs: d8bebcb0abd89193874c59ed3023f5b4f81b89b6676d187ad7fbdb0e
# Bytes stcs: 3f1d9bd2f8c3d7d8144b789433261370eaf25cdc83fe8a745ef880c1
# Der PubKeyHash belongs to my address: addr_test1qqt86eq9972q3qttj6ztje97llasktzfzvhmdccqjlqjaq2cer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qy6q5t2


# Get UtxO's
# cardano-cli query utxo --address addr_test1qqt86eq9972q3qttj6ztje97llasktzfzvhmdccqjlqjaq2cer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qy6q5t2 --testnet-magic 2


# Calculate Fee
# cardano-cli transaction calculate-min-fee --tx-body-file stake_unsigned.tx --testnet-magic 2 --protocol-params-file protocolparams.json --tx-in-count 3 --tx-out-count 2 --witness-count 2
#

# Sign with eternl

# Create a Witness file
# Use the existing witness file and replace the signature, attention:!!! You need to erase the first 6 bytes, eternl has wrappen the signature as TxWitness !!! 
# This witness has correct length as comparsion:
# 825820fd8b0d639478778be68d760f1169e69264a29a2e19e9aec4be02f00d4db3baaf5840e75788d6d253495e5e2989d46f341db34ee87c629995a7a043bc6bf0566aba373a6fe201c2508cdf27f2ccc70eda5f864f6e3b5cd178bb35a33428836842be0a

# Then Assemble tx body and witness
# cardano-cli transaction assemble --tx-body-file stake_unsigned_build_raw.tx --witness-file witness.tx --out-file signed.tx

# Then Submit to network
# cardano-cli transaction submit --tx-file signed.tx --testnet-magic 2

# Get TxHash
# cardano-cli transaction txid --tx-file signed.tx
