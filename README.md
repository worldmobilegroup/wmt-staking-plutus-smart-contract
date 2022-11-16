# earthnode-registration-smart-contract
Earth Node Registration Smart Contract

## Generate a contract instance
cabal run en-registration `NFT PolicyID` `Admin Wallet Address` `Network Magic`

Example: 
```cabal run en-registration  14696a4676909f4e3cb1f2e60e2e08e5abed70caf5c02699be971139 addr_test1qqgagc0fy6nm0qe4h8zqxsg952tqjeg7l7j0agd0cx4u25zcer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qxvept2 2```

Output: `{"plutus_file":"en-nft-registration.plutus","script_address":"addr_test1wp8fpqkglhvlrke0f60p84cf59ek678gsj85ghvcz6a4quqfkfv4e"}`

### Testnet | Mainnet
To generate a smart contract instance for mainnet provide a 0 as last argument. If you provide any other number than zero you create a testnet contract with the magic number you entered (Preprod: 1, Preview: 2, Legacy Testnet: 1097911063)

## Setup Plutus Environment

coming soon...