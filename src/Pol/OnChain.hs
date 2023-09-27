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
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}

module Pol.OnChain
  ( vUt
  ) where
-- import           Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Value    as Value (flattenValue,valueOf,unAssetClass, AssetClass)
import           Plutus.V1.Ledger.Address  (scriptHashAddress)
import           Plutus.V2.Ledger.Contexts as V2 ()
import           PlutusTx.Prelude               
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts

import           Pol.Types

{-- Validate Unstaking --}
-- Checks ony if the proof of execution is burnt, the rest is checked by the wmt-staking smart contract
-- We check that the CurrencySymbol of the token is the one of this minting policy
{-# INLINABLE validateUnstaking #-}
validateUnstaking :: ScriptParams -> CurrencySymbol -> TxInfo -> ScriptPurpose -> Bool
validateUnstaking sp ocs info (Minting cs)
    | proofOfExecutionBurnt cs (getExecutionProofTN sp info) $ txInfoMint info, ocs == cs = True
    | otherwise = False
validateUnstaking _ _ _ _ = False

-- Make sure the Token is burnt
{-# INLINABLE proofOfExecutionBurnt #-}
proofOfExecutionBurnt :: CurrencySymbol -> TokenName -> Value -> Bool
proofOfExecutionBurnt cs tn v =
  let 
    burn_amt = Value.valueOf v cs tn
  in 
    burn_amt == -1

-- We need to determine the token programmatically, for that we reconstruct the tokenname from
-- the input UTxO which is spent from the wmt staking smart contract and shall contain this token
{-# INLINABLE getExecutionProofTN #-}
getExecutionProofTN :: ScriptParams -> TxInfo -> TokenName
getExecutionProofTN ScriptParams{..} info =  
  let
    orf = txOutFromWmtSC (txInfoInputs info) spWMT spWmtStakingContr 
  in
    TokenName $ consByteString (txOutRefIdx orf) (appendByteString (getTxId $ txOutRefId orf) "#" ) 

-- We determine the TxOutRef of the input from the wmt staking smart contract, we also know it must have at least 1 WMT in the UTxO to be valid
{-# INLINABLE txOutFromWmtSC #-}
txOutFromWmtSC :: [TxInInfo] -> Value.AssetClass -> ValidatorHash -> TxOutRef
txOutFromWmtSC is a vh = 
  let 
    filter' :: Value.AssetClass -> TxInInfo -> [TxOutRef]
    filter' ac i | (Value.valueOf (txOutValue o) wmt_cs wmt_tn) > 1 && b == ScriptCredential vh = [txInInfoOutRef i]
                     | otherwise = []
                     where 
                      (wmt_cs, wmt_tn) = Value.unAssetClass ac
                      o = txInInfoResolved i
                      b = addressCredential $ txOutAddress o 

    orefs [] = []
    orefs (x:xs) = filter' a x ++ orefs xs
  in
    case orefs is of
      [h] -> h
      _ -> error () --"more than one script input or none"
      

{-- Validate Staking --}
-- Validates that the proof of execution is minted only if the staking conditions are met
-- Makes sure the proof of execution is paid to the wmt-staking smart contract and contains also the
-- amount of WMT specified in the redeemer
{-# INLINABLE validateStaking #-}
validateStaking :: ScriptParams -> CurrencySymbol -> StakeAmount -> TxInfo -> ScriptPurpose -> Bool
validateStaking sp ocs samt info (Minting cs)
    | valuePaidToScript sp ocs info samt, checkEarthNode sp info, ocs == cs = True
    | otherwise = False
validateStaking _ _ _ _ _ = False

-- Helper Functions
--

-- We ensure that the proof of execution as well as the specified amount of WMT is spent to the wmt staking smart contract
-- Script Parameter -> MintingPolicy Currency Symbol (own) -> TxInfo -> Wmt Staking Script Hash -> Redeemer stake amount -> Bool
{-# INLINABLE valuePaidToScript #-}
valuePaidToScript :: ScriptParams -> CurrencySymbol -> TxInfo -> StakeAmount -> Bool
valuePaidToScript ScriptParams{..} ocs info samt = 
  let 
    v = valueLockedBy info spWmtStakingContr
    -- We construct an individual token name for each staker and each individual staking action 
    -- To achieve this we use the TxId of the creating transaction (this transaction) and TxOutIndex of the TxOut paid to the wmt staking script
    tn = TokenName $ consByteString (txIdToWmtSC info spWMT samt spWmtStakingContr) (appendByteString (getTxId $ txInfoId info) "#" ) 
    (wmt_cs, wmt_tn) = Value.unAssetClass spWMT
    lwmt = Value.valueOf v wmt_cs wmt_tn
    vp = valueProduced info 
    lep = Value.valueOf vp ocs tn
    lep' = Value.valueOf v ocs tn
  in
    lwmt == samt && lep == 1 && lep' == lep



-- Get TxId of the output spent to a smart contract
-- We do this to programatically generate and check for the tokenname of the proof of execution
{-# INLINABLE txIdToWmtSC #-}
txIdToWmtSC :: TxInfo -> Value.AssetClass -> StakeAmount -> ValidatorHash -> Integer
txIdToWmtSC info a samt vh = 
  let 
    txout :: [TxOut]
    txout = txInfoOutputs info
  
    filter'' :: Value.AssetClass -> TxOut -> Bool
    filter'' ac o | (Value.valueOf (txOutValue o) wmt_cs wmt_tn) == samt && (txOutAddress o) == (scriptHashAddress vh) = True
                     | otherwise = False
                     where 
                      (wmt_cs, wmt_tn) = Value.unAssetClass ac
    index :: Integer -> [TxOut] -> [Integer]
    index _ [] = []
    index n (x:xs) | filter'' a x = n : index (n + 1::Integer) xs
    index _ _ = []
  in
    case index 0 txout of
      [h] -> h
      _ -> error () --"more than one script output or none"
      
    
-- We verify that the EarthNodeNFT is present in the reference Inputs and it is a legit asset
-- We also ensure that the Datum is formated correctly and is a RegistrationDatum
{-# INLINABLE checkEarthNode #-}
checkEarthNode :: ScriptParams -> TxInfo -> Bool
checkEarthNode ScriptParams{..} info = 
  let
    os = getRegistrationReferenceInputs spNftCs spRegContr $ txInfoReferenceInputs info
  in
    case os of 
      [_] -> True
      _ -> error () -- no reference input present or several, which is not allowed

-- Get the TxOut's from the inputs where the Address belongs to a ValidatorHash and also verify there is an NFT present on the
-- input UTxO which machtes the corresponding Currency Symbol
{-# INLINABLE getRegistrationReferenceInputs #-}
getRegistrationReferenceInputs :: CurrencySymbol -> ValidatorHash -> [TxInInfo] -> [TxOut]
getRegistrationReferenceInputs _ _ [] = []
getRegistrationReferenceInputs pcs pvh (h:t) =
  let
    checkReferenceInput TxInInfo{txInInfoResolved=out@TxOut{txOutAddress=Address (ScriptCredential vh) _, txOutValue, txOutDatum=OutputDatum (Datum d)} } | pvh == vh && (isRegistrationDatum d) && (isJust $ ennftInValue pcs txOutValue) = [out]
                                                                                                                                                              | otherwise = []
    checkReferenceInput _                                                                                                                                     = []
  in
    checkReferenceInput h ++ (getRegistrationReferenceInputs pcs pvh t)


-- Check if some Datum (BuiltinData) is a EnRegistration Datum
{-# INLINABLE isRegistrationDatum #-}
isRegistrationDatum :: BuiltinData -> Bool
isRegistrationDatum d = 
  let 
    getEnRegDat :: EnRegistration -> Bool
    getEnRegDat  EnRegistration {} = True
--    getEnRegDat  _ = False
  in
    getEnRegDat (unsafeFromBuiltinData  d)

-- Check if there is an Specific NFT in a given Value and return the TokenName
{-# INLINABLE ennftInValue #-}
ennftInValue :: CurrencySymbol -> Value -> Maybe TokenName
ennftInValue cs v = 
  let
    getENNFTName :: (CurrencySymbol, TokenName, Integer) -> Maybe TokenName
    getENNFTName ( cs', t, amt  ) | cs' == cs && amt == 1 = Just t
                                  | otherwise = Nothing

    handleList :: [(CurrencySymbol, TokenName, Integer)] -> Maybe TokenName 
    handleList [] = Nothing
    handleList (t:ts) = case getENNFTName t of 
      Just t' -> Just t'
      Nothing -> handleList ts
  in
    handleList $ Value.flattenValue v

-- Main Mintin Policy
{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> Action -> ScriptContext -> Bool
mkVal sp (Stake samt) ctx = validateStaking sp (ownCurrencySymbol ctx) samt (scriptContextTxInfo ctx) $ scriptContextPurpose ctx
mkVal sp UnStake ctx = validateUnstaking sp (ownCurrencySymbol ctx) (scriptContextTxInfo ctx) $ scriptContextPurpose ctx

{-# INLINABLE vUt #-}
vUt :: BuiltinData -> BuiltinData -> BuiltinData -> ()
vUt s r c =
   wVal mkVal (unsafeFromBuiltinData s) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

{-# INLINABLE wVal #-}
wVal :: forall s r c
    . (UnsafeFromData s, UnsafeFromData r, UnsafeFromData c)
    => (s -> r -> c -> Bool)
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wVal f s r c = check (f (unsafeFromBuiltinData s) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c))
