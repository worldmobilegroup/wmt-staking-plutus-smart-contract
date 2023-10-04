{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v0.1
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
{-# LANGUAGE BangPatterns          #-}

module Val.Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           Ledger
import GHC.Generics (Generic)
import           Plutus.V2.Ledger.Api
import qualified PlutusTx
import           PlutusTx.Prelude     ()
import qualified Prelude

data ScriptParams = ScriptParams
        {
              pNftCs        :: CurrencySymbol
            , pRegContract  :: ValidatorHash
            , pStCs         :: CurrencySymbol
            , pStTn         :: TokenName
            , pAdm          :: PubKeyHash
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.unstableMakeIsData ''ScriptParams
PlutusTx.makeLift ''ScriptParams


data WmtStaking = WmtStaking
        {
              swmtENNFT     :: TokenName
            , swmtOwner     :: PubKeyHash
            , sExPrCs       :: CurrencySymbol
            , sExprTn       :: TokenName
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''WmtStaking [('WmtStaking, 0)]
PlutusTx.makeLift ''WmtStaking

data Action = UnStake | Admin
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''Action [('UnStake, 0),('Admin, 1)]
PlutusTx.makeLift ''Action