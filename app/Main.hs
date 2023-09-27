{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v0.1
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}

import           Cardano.Api
import           Cardano.Api.Shelley          (Address (ShelleyAddress))
import           Cardano.Ledger.Alonzo.TxInfo (transKeyHash,transScriptHash)
import           Cardano.Ledger.Shelley.API   (Credential (KeyHashObj,ScriptHashObj))
import           Data.ByteString.UTF8         (fromString)
import           Data.Text
import           Ledger
import           Plutus.Contract.CardanoAPI   (toCardanoAddressInEra)
import           Plutus.V1.Ledger.Value       (currencySymbol,tokenName,assetClass)
import qualified PlutusTx
import           Prelude
import           System.Environment           (getArgs)
import           Val.OffChain               as ValOffChain
import           Val.Types                  as ValTypes
import           Pol.OffChain               as PolOffChain
import           Pol.Types                  as PolTypes

main :: IO ()
main = do
    args <- getArgs
    case args of

      [nftcs',stcs',sttn',vh_reg',adm',magic'] -> do
        let
            validatorFile   = "wmt_staking_dummy.plutus"
            policyFile = "wmt-staking-proof-of-execution_dummy.plutus"
            vh_reg          = addrToVh $ either (\_ -> error "Not a valid validator address") id (parseShelleyAddr vh_reg')
            pkh          = addrToPkh $ either (\_ -> error "Not a valid address") id (parseShelleyAddr adm')
            nftcs = currencySymbol $ fromString nftcs'
            stcs = currencySymbol $ fromString stcs'
            sttn = tokenName $ fromString sttn'
            validatorParams          = ValTypes.ScriptParams
                {
                    pNftCs       = nftcs
                  , pRegContract = vh_reg
                  , pStCs        = stcs
                  , pStTn        = sttn
                  , pAdm         = pkh
                }
            magic = case read magic' :: Integer of        -- 1..n: (Testnet (NetworkMagic n)) || 0: Mainnet
              0 -> Mainnet
              x -> Testnet (NetworkMagic $ fromInteger x)
            address     = scriptAddress validatorParams
            address'    = case toCardanoAddressInEra magic address of
                            Left err    -> error $ "cannot create bech32 contract address: " ++ show err
                            Right addr' -> serialiseAddress addr'
            vh_st       = ValOffChain.scriptHash $ PlutusTx.toBuiltinData validatorParams
            policyParams          = PolTypes.ScriptParams
                {
                    spNftCs            = nftcs
                  , spWMT              = assetClass stcs sttn
                  , spRegContr         = vh_reg
                  , spWmtStakingContr  = vh_st
                }
        valResult <- writeFileTextEnvelope validatorFile Nothing $ ValOffChain.apiScript validatorParams
        polResult <- writeFileTextEnvelope policyFile Nothing $ PolOffChain.apiScript policyParams

        case valResult of
            Left err -> print $ displayError err
            Right () -> case polResult of
              Left err -> print $ displayError err
              Right () -> Prelude.putStrLn $ "{\"plutus_file_policy\":" ++ show policyFile ++ ",\"plutus_file_validator\":" ++ show validatorFile ++ ",\"validator_address\":" ++ show address' ++ "}"
      _ -> error "You need to provide a NFT curency symbol, a staking token CurrencySymbol, a staking token TokenName, the Script Address of the registration contract, the admin wallet address and the networkmagic (0 == mainnet)!"

-- read and decode bech32 Cardano address
parseShelleyAddr :: String -> Either Bech32DecodeError (Cardano.Api.Shelley.Address ShelleyAddr)
parseShelleyAddr s = deserialiseFromBech32 AsShelleyAddress $ Data.Text.pack s

-- get PKH from bech32 address
addrToPkh :: Cardano.Api.Shelley.Address ShelleyAddr -> PubKeyHash
addrToPkh (ShelleyAddress _ (KeyHashObj kh) _) = transKeyHash kh
addrToPkh _                                    = error "addrToPkh"

-- get VH from bech32 address
addrToVh :: Cardano.Api.Shelley.Address ShelleyAddr -> ValidatorHash
addrToVh (ShelleyAddress _ (ScriptHashObj kh) _) = transScriptHash kh
addrToVh _                                    = error "addrToVh"