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
--import           Data.ByteString              as B
import           Data.ByteString.UTF8         as BSU
--import           Data.String                  (IsString (..))
import           Data.Text
import           Ledger
--import           Ledger.Address
--import           Ledger.Bytes                 (getLedgerBytes)
--import           Ledger.Tx.CardanoAPI
import           OffChain
import           Plutus.V1.Ledger.Value       (currencySymbol)
--import           Plutus.V2.Ledger.Api         (PubKeyHash)
--import           PlutusTx.Builtins            (BuiltinByteString, BuiltinData)
import           Prelude
import           System.Environment           (getArgs)
import           Types

main :: IO ()
main = do
    args <- getArgs
    case args of

      [cs',adm'] -> do
        let
            scriptFile   = "en-nft-registration.plutus"
            pkh          = addrToPkh $ either (\_ -> error "Not a valid address") id (parseShelleyAddr adm')
            cs = currencySymbol $ BSU.fromString cs' -- "14696a4676909f4e3cb1f2e60e2e08e5abed70caf5c02699be971139"
            params          = ScriptParams
                {
                    pNftCs       = cs -- read fee' :: Integer --25      -- 2.5 percent fee of selling price
                  , adm          = pkh
                } -- $ ToBuiltin $ ByteString

            address     = escrowAddress params
            --address'    = Data.Text.unpack $ serialiseToBech32 $ toCardanoAddress address
        scriptResult <- writeFileTextEnvelope scriptFile Nothing $ apiScript params
        case scriptResult of
            Left err -> print $ displayError err
            Right () -> Prelude.putStrLn $ "wrote marketplace contract to file " ++ show scriptFile ++ " script address: " ++ show address
      _ -> error "You need to provide a curency symbol and an admin address!"
--parsePkh :: String -> PubKeyHash
--parsePkh s = PubKeyHash $ getLedgerBytes $ Data.String.fromString s

parseShelleyAddr :: String -> Either Bech32DecodeError (Cardano.Api.Shelley.Address ShelleyAddr)
parseShelleyAddr s = deserialiseFromBech32 AsShelleyAddress $ Data.Text.pack s

addrToPkh :: Cardano.Api.Shelley.Address ShelleyAddr -> PubKeyHash
--addrToPkh (ShelleyAddress net (ScriptHashObj sh) sr) = transScriptHash sh -> ValidatorHash
addrToPkh (ShelleyAddress _ (KeyHashObj kh) _) = transKeyHash kh
addrToPkh _                                    = error "addrToPkh"
--strToCs :: String -> CurrencySymbol
--strToCs s =
