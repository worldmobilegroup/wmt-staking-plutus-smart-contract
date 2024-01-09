{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v1.0
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
import qualified Plutus.V1.Ledger.Value    as Value (valueOf,unAssetClass,AssetClass)
import           Plutus.V2.Ledger.Contexts as V2 ()
import           PlutusTx.Prelude
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           Val.Types                hiding (ScriptParams,Action)
import           Pol.Types


-- Checks only if the proof of execution is burnt, the rest is checked by the wmt-staking smart contract
-- We check that the CurrencySymbol of the token is the one of this minting policy
{-# INLINABLE validateUnstaking #-}
validateUnstaking :: ScriptParams -> CurrencySymbol -> TokenName -> TxInfo -> ScriptPurpose -> Bool
validateUnstaking sp ocs tn info (Minting cs)
    |   traceIfFalse "proofOfExecutionBurnt" $ proofOfExecutionBurnt cs tn $ txInfoMint info
      , traceIfFalse "getExecutionProofTN" $ getExecutionProofTN ocs tn sp info, ocs == cs = True
    | otherwise = False
validateUnstaking _ _ _ _ _ = False


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
getExecutionProofTN :: CurrencySymbol -> TokenName -> ScriptParams -> TxInfo -> Bool
getExecutionProofTN ocs tn ScriptParams{..} info =
  let
    v = txOutFromWmtSC (txInfoInputs info) spWMT spWmtStakingContr
  in
    Value.valueOf v ocs tn == 1

-- We determine the TxOutRef of the input from the wmt staking smart contract, we also know it must have at least 1 WMT in the UTxO to be valid
{-# INLINABLE txOutFromWmtSC #-}
txOutFromWmtSC :: [TxInInfo] -> Value.AssetClass -> ValidatorHash -> Value
txOutFromWmtSC is a vh =
  let
    filter' :: Value.AssetClass -> TxInInfo -> [TxOut]
    filter' ac i | Value.valueOf (txOutValue o) wmt_cs wmt_tn > 1 && b == ScriptCredential vh = [o]
                     | otherwise = []
                     where
                      (wmt_cs, wmt_tn) = Value.unAssetClass ac
                      o = txInInfoResolved i
                      b = addressCredential $ txOutAddress o

    os [] = []
    os (x:xs) = filter' a x ++ os xs
  in
    case os is of
      [h] -> txOutValue h
      _ -> traceError "more than one script input or none"


{-- Validate Staking --}
-- Validates that the proof of execution is minted only if the staking conditions are met
-- Makes sure the proof of execution is paid to the wmt-staking smart contract and contains also the
-- amount of WMT specified in the redeemer
{-# INLINABLE validateStaking #-}
validateStaking :: ScriptParams -> CurrencySymbol -> Integer -> TokenName -> Integer -> TxInfo -> Bool
validateStaking sp ocs samt tn idx info
    |   traceIfFalse "checkTnUTxOSpent" $ checkTnUTxOSpent tn idx info
      , traceIfFalse "valuePaidToScript" $ valuePaidToScript sp ocs tn info samt = True
    | otherwise = traceIfFalse "otherwise" False

-- Helper Functions
--

-- We ensure that the proof of execution as well as the specified amount of WMT is spent to the wmt staking smart contract
{-# INLINABLE valuePaidToScript #-}
valuePaidToScript :: ScriptParams -> CurrencySymbol -> TokenName -> TxInfo -> Integer -> Bool
valuePaidToScript sp@ScriptParams{..} ocs tn info samt =
  let
    (cs',tn') = Value.unAssetClass spWMT
    so = scriptOutputsAt spWmtStakingContr info
    (d,v) = case so of
      [] -> traceError "could not find staking validator outputs"
      [(OutputDatum (Datum od),va)] -> trace "va" (od,va)
      _ -> traceError "more than one staking validator output"
    vp = valueProduced info
    lep = Value.valueOf vp ocs tn == Value.valueOf v ocs tn
    lst = trace (appendString "Could not find ENNFT in reference input: " (decodeUtf8 (unTokenName tn'))) $ Value.valueOf v cs' tn' >= samt
    earthdat = checkEarthNode sp info
    stdat = isStakingDatum ocs tn d earthdat info
  in
       traceIfFalse "exp token not minted or spent" lep
    && traceIfFalse "lst != samt" lst
    && traceIfFalse "stake datum checks failed" stdat

-- Check staking datum
{-# INLINABLE isStakingDatum #-}
isStakingDatum :: CurrencySymbol -> TokenName -> BuiltinData -> BuiltinData -> TxInfo -> Bool
isStakingDatum ocs tn d ed info =
  let
    stakingDat :: WmtStaking -> EnRegistration -> Bool
    stakingDat WmtStaking{..} EnRegistration{..} =     traceIfFalse "Wrong sExprTn" (sExprTn == tn)
                                                      && traceIfFalse "Wrong sExPrCs" (sExPrCs == ocs)
                                                      && traceIfFalse "Wrong swmtENNFT" (swmtENNFT == enUsedNftTn)
                                                      && traceIfFalse "Wrong signature" (txSignedBy info swmtOwner)
  in
    stakingDat (unsafeFromBuiltinData d) $ unsafeFromBuiltinData ed

-- We check that the tokenname is equal to an input UTxO txid+index and that this input is spent to make sure the tokenname is unique
{-# INLINABLE checkTnUTxOSpent #-}
checkTnUTxOSpent :: TokenName -> Integer -> TxInfo -> Bool
checkTnUTxOSpent tn i info =
  let
    txid = TxId $ unTokenName tn
  in
    traceIfFalse "tn output not spent" $ spendsOutput info txid i -- Check if this element is going to be spent

-- We verify that the EarthNodeNFT is present in the reference Inputs and it is a legit asset
-- We also ensure that the Datum is formated correctly and is a RegistrationDatum
{-# INLINABLE checkEarthNode #-}
checkEarthNode :: ScriptParams -> TxInfo -> BuiltinData
checkEarthNode sp info =
  let
    os = getRegistrationReferenceInputs sp $ map txInInfoResolved $ txInfoReferenceInputs info
  in
    case os of
      [d] -> d
      _ -> traceError "check EarthNode failed" -- no reference input present or several, which is not allowed

-- Get the TxOut's from the inputs where the Address belongs to a ValidatorHash and also verify there is an NFT present on the
-- input UTxO which machtes the corresponding Currency Symbol
{-# INLINABLE getRegistrationReferenceInputs #-}
getRegistrationReferenceInputs :: ScriptParams -> [TxOut] -> [BuiltinData]
getRegistrationReferenceInputs _ [] = []
getRegistrationReferenceInputs sp@ScriptParams{..} (h:t) =
  let
    checkReferenceInput TxOut{txOutAddress=Address (ScriptCredential vh) _, txOutValue, txOutDatum=OutputDatum (Datum d)}  | traceIfFalse "Wrong Registration SC" (spRegContr == vh) && traceIfFalse "problem with reference input" (isRegistrationDatum sp txOutValue d) = [d] -- && traceIfFalse "no ENNFT present" (ennftInValue pcs txOutValue) 
                                                                                                                                                          | otherwise = []
    checkReferenceInput _                                                                                                                                  = trace "could not match TxIn" []
  in
    checkReferenceInput h ++ getRegistrationReferenceInputs sp t


-- Check if some Datum (BuiltinData) is a EnRegistration Datum
{-# INLINABLE isRegistrationDatum #-}
isRegistrationDatum :: ScriptParams -> Value -> BuiltinData -> Bool
isRegistrationDatum ScriptParams{..} v d =
  let
    getEnRegDat :: EnRegistration -> Bool
    getEnRegDat  EnRegistration {..} = traceIfFalse "Could not find ENNFT in reference input" $ Value.valueOf v spNftCs enUsedNftTn == 1
  in
    getEnRegDat $ unsafeFromBuiltinData d

-- Main Mintin Policy
{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> Action -> ScriptContext -> Bool
mkVal sp (Stake samt tn idx) ctx = validateStaking sp (ownCurrencySymbol ctx) samt tn idx (scriptContextTxInfo ctx)
mkVal sp (Pol.Types.UnStake tn) ctx = validateUnstaking sp (ownCurrencySymbol ctx) tn (scriptContextTxInfo ctx) $ scriptContextPurpose ctx

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
