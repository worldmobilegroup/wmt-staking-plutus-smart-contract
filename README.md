# WMT Staking Smart Contract
WMT Staking Smart Contract

## Generate a contract instance
cabal run en-registration `Staking Token Tokenname` `Registration Contract Address` `Admin Wallet Address` `Network Magic`

Example: 
```
cabal run wmt-staking tokenname addr_test1wpp8knh2x50mfhmhs0ge8mjpkcwhxfp2k8gmflqdcs34n4g3mrp3l addr_test1qqgagc0fy6nm0qe4h8zqxsg952tqjeg7l7j0agd0cx4u25zcer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qxvept2 2
```

Output: 
```
{"plutus_file_policy":"wmt-staking-proof-of-execution_dummy.plutus","plutus_file_validator":"wmt_staking_dummy.plutus","validator_address":"addr_test1wq0xp80pntp0q93j4tmage3jtdq0mzv7krx2ktd6j0gpzpg60lu2r"}
```

### Testnet | Mainnet
To generate a smart contract instance for mainnet provide a 0 as last argument. If you provide any other number than zero you create a testnet contract with the magic number you entered (Preprod: 1, Preview: 2, Legacy Testnet: 1097911063)
