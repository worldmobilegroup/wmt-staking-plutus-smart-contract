{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2022
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
import           Cardano.Ledger.Alonzo.TxInfo (transKeyHash)
import           Cardano.Ledger.Shelley.API   (Credential (KeyHashObj))
import           Data.ByteString.UTF8         (fromString)
import           Data.Text
import           Ledger
import           OffChain
import           Plutus.Contract.CardanoAPI   (toCardanoAddressInEra)
import           Plutus.V1.Ledger.Value       (currencySymbol)
import           Prelude
import           System.Environment           (getArgs)
import           Types

main :: IO ()
main = do
    args <- getArgs
    case args of

      [cs',adm',magic'] -> do
        let
            scriptFile   = "en-nft-registration.plutus"
            pkh          = addrToPkh $ either (\_ -> error "Not a valid address") id (parseShelleyAddr adm') -- "addr_test1qqgagc0fy6nm0qe4h8zqxsg952tqjeg7l7j0agd0cx4u25zcer3t74yn0dm8xqnr7rtwhkqcrpsmphwcf0mlmn39ry6qxvept2"
            cs = currencySymbol $ fromString cs' -- "14696a4676909f4e3cb1f2e60e2e08e5abed70caf5c02699be971139"
            params          = ScriptParams
                {
                    pNftCs       = cs  -- CurrencySymbol / PolicyID
                  , adm          = pkh -- Admin wallet address
                }
            magic = case read magic' :: Integer of        -- 1..n: (Testnet (NetworkMagic n)) || 0: Mainnet
              0 -> Mainnet
              x -> Testnet (NetworkMagic $ fromInteger x)
            address     = scriptAddress params
            address'    = case toCardanoAddressInEra magic address of
                            Left err    -> error $ "cannot create bech32 contract address: " ++ show err
                            Right addr' -> serialiseAddress addr'
        scriptResult <- writeFileTextEnvelope scriptFile Nothing $ apiScript params
        case scriptResult of
            Left err -> print $ displayError err
            Right () -> Prelude.putStrLn $ "{\"plutus_file\":" ++ show scriptFile ++ ",\"script_address\":" ++ show address' ++ "}"
      _ -> error "You need to provide a curency symbol and an admin address!"

-- read and decode bech32 Cardano address
parseShelleyAddr :: String -> Either Bech32DecodeError (Cardano.Api.Shelley.Address ShelleyAddr)
parseShelleyAddr s = deserialiseFromBech32 AsShelleyAddress $ Data.Text.pack s

-- get PKH from bech32 address
addrToPkh :: Cardano.Api.Shelley.Address ShelleyAddr -> PubKeyHash
addrToPkh (ShelleyAddress _ (KeyHashObj kh) _) = transKeyHash kh
addrToPkh _                                    = error "addrToPkh"
