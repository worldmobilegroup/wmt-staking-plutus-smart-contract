{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2022
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

module Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           Ledger
import           Playground.Contract  (Generic)
--import           Plutus.V2.Ledger.Api (Map) --Credential
import           Plutus.V2.Ledger.Api
import qualified PlutusTx
import           PlutusTx.Prelude     ()
import qualified Prelude

data ScriptParams = ScriptParams
        {
              pNftCs :: CurrencySymbol
            , adm    :: PubKeyHash
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.unstableMakeIsData ''ScriptParams
PlutusTx.makeLift ''ScriptParams

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

data Action = Register | Unregister | Admin
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''Action [('Register, 0),('Unregister, 1),('Admin, 2)]
PlutusTx.makeLift ''Action

{-
data AAddress = AAddress
  {
      aaddressCredential        :: Credential
    , aaddressStakingCredential :: BuiltinData
  }
PlutusTx.makeIsDataIndexed ''AAddress [('AAddress,0)]
PlutusTx.makeLift ''AAddress

data AOutputDatum = NoOutputDatum | OutputDatumHash DatumHash | OutputDatum Datum
PlutusTx.makeIsDataIndexed ''AOutputDatum [('NoOutputDatum,0), ('OutputDatumHash,1), ('OutputDatum,2)]
PlutusTx.makeLift ''AOutputDatum

data ATxOut = ATxOut
  {
      atxOutAddress         :: AAddress
    , atxOutValue           :: Value
    , atxOutDatumHash       :: AOutputDatum
    , atxOutReferenceScript :: BuiltinData
  }
PlutusTx.makeIsDataIndexed ''ATxOut [('ATxOut,0)]
PlutusTx.makeLift ''ATxOut

data ATxInInfo = ATxInInfo
  {
      atxInInfoOutRef   :: BuiltinData
    , atxInInfoResolved :: ATxOut

  }
PlutusTx.makeIsDataIndexed ''ATxInInfo [('ATxInInfo,0)]
PlutusTx.makeLift ''ATxInInfo

data ATxInfo = ATxInfo {
      atxInfoInputs          :: [ATxInInfo] --ATxInInfo
    , atxInfoReferenceInputs :: [ATxInInfo] --ATxInInfo
    , atxInfoOutputs         :: [ATxOut]
    , atxInfoFee             :: BuiltinData
    , atxInfoMint            :: BuiltinData
    , atxInfoDCert           :: BuiltinData
    , atxInfoWdrl            :: BuiltinData
    , atxInfoValidRange      :: BuiltinData
    , atxInfoSignatories     :: [PubKeyHash]
    , atxInfoRedeemers       :: BuiltinData
    , atxInfoData            :: Map DatumHash Datum
    , atxInfoId              :: BuiltinData
}
PlutusTx.makeIsDataIndexed ''ATxInfo [('ATxInfo,0)]
PlutusTx.makeLift ''ATxInfo

data AScriptContext = AScriptContext
  { aScriptContextTxInfo :: ATxInfo
  , scriptContextPurpose :: BuiltinData
  }
PlutusTx.makeIsDataIndexed ''AScriptContext [('AScriptContext,0)]
PlutusTx.makeLift ''AScriptContext
-}
