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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
{-# LANGUAGE BangPatterns          #-}

module Pol.Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           Ledger
import           GHC.Generics         (Generic)
import           Plutus.V2.Ledger.Api
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude     
import qualified Prelude              

data EnRegistration = EnRegistration
        {
              enOperatorAddress :: BuiltinByteString
            , enConsensusPubKey :: BuiltinByteString
            , enMerkleTreeRoot  :: BuiltinByteString
            , enCceAddress      :: BuiltinByteString
            , enUsedNftTn       :: TokenName
            , enOwner           :: PubKeyHash
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''EnRegistration [('EnRegistration, 0)]
PlutusTx.makeLift ''EnRegistration

data ScriptParams = ScriptParams
        {
              spNftCs            :: CurrencySymbol
            , spWMT              :: AssetClass
            , spRegContr         :: ValidatorHash
            , spWmtStakingContr  :: ValidatorHash
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.unstableMakeIsData ''ScriptParams
PlutusTx.makeLift ''ScriptParams

data Action = Stake Integer TokenName Integer | UnStake TokenName 
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''Action [('Stake, 0),('UnStake, 1)]
PlutusTx.makeLift ''Action
