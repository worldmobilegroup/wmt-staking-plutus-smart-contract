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
import           Plutus.V2.Ledger.Api
import qualified PlutusTx
import           Prelude
import           System.Environment           (getArgs)
import           Val.OffChain               as ValOffChain
import           Val.Types                  as ValTypes
import           Pol.OffChain               as PolOffChain
import           Pol.Types                  as PolTypes

import qualified PlutusTx.Prelude       as API

import Data.ByteString.Lazy as BL
import Data.ByteString as BS
import Data.Text as TS
import Data.Text.Lazy as TL
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.ByteString.UTF8 as BSU      -- from utf8-string
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE
import Data.Word8 as W8

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
            pkh_print    = addrToPkh $ either (\_ -> error "Not a valid address") id (parseShelleyAddr "addr_test1qqt86eq9972q3qttj6ztje97llasktzfzvhmdccqjlqjaq2cer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qy6q5t2")
            nftcs = currencySymbol $ BS.pack ([216,190,188,176,171,216,145,147,135,76,89,237,48,35,245,180,248,27,137,182,103,109,24,122,215,251,219,14::W8.Word8])
            --currencySymbol$ BSU.fromString nftcs'
            -- CurrencySymbol $ getLedgerBytes $ fromBytes $ BS.pack ([216,190,188,176,171,216,145,147,135,76,89,237,48,35,245,180,248,27,137,182,103,109,24,122,215,251,219,224::W8.Word8])
            -- ([64,38,62,65,62,63,62,30,61,62,64,38,39,31,39,33,38,37,34,63,35,39,65,64,33,30,32,33,66,35,62,34,66,38,31,62,38,39,62,36,36,37,36,64,31,38,37,61,64,37,66,62,64,62,30,65::W8.Word8]) 
            --BS.pack $ BS.unpack $ BSU.fromString nftcs' --currencySymbol$ fromString nftcs' --
            stcs = currencySymbol $ BS.pack ([63,29,155,210,248,195,215,216,20,75,120,148,51,38,19,112,234,242,92,220,131,254,138,116,94,248,128,193::W8.Word8])
            sttn = tokenName $ BSU.fromString sttn'
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
            ocs = PolOffChain.mustMintPolicyCurrencySymbol $ PlutusTx.toBuiltinData policyParams
        valResult <- writeFileTextEnvelope validatorFile Nothing $ ValOffChain.apiScript validatorParams
        polResult <- writeFileTextEnvelope policyFile Nothing $ PolOffChain.apiScript policyParams

        case valResult of
            Left err -> print $ displayError err
            Right () -> case polResult of
              Left err -> print $ displayError err
              Right () -> Prelude.putStrLn $ "{\"plutus_file_policy\":" ++ show policyFile ++ ",\"currencysymbol\":\"" ++ show ocs ++ "\",\"plutus_file_validator\":" ++ show validatorFile ++ ",\"validator_address\":" ++ show address' ++ "}" ++ "\nPubKeyHash" ++ show pkh_print ++ "\n" ++ "Bytes nftcs: " ++ show nftcs ++"\n" ++ "Bytes stcs: " ++ show stcs
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

--toAsciiByte :: [Char] -> [W8.Word8]
--toAsciiByte [] = []
--toAsciiByte (h:t) = fromEnum h:toAsciiByte t
