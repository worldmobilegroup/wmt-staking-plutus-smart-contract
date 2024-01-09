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

module Val.OnChain
  ( vUt
  ) where
import qualified Plutus.V1.Ledger.Value    as Value (valueOf)
import           Plutus.V2.Ledger.Contexts as V2 (findOwnInput,getContinuingOutputs)
import           PlutusTx.Prelude
import           Plutus.V2.Ledger.Api

import           Val.Types

-- Validate Admin
-- It is generally possible to deadlock if someone tries to stake with a different minitng policy as execution proof or using another NFT as execution proof.
-- This is a missuse which cannot lead to valid staking transaction but we might want to help those users to unlock their tokens again.
{-# INLINABLE validateAdmin #-}
validateAdmin :: ScriptParams -> ScriptContext -> Bool
validateAdmin ScriptParams{..} ctx
    | txSignedBy' (txInfoSignatories info) pAdm = True
    | otherwise = False
    where
        info = scriptContextTxInfo ctx

-- Validate Unstaking
-- It is only possible to unstake a full UTxO, a partial unstake of WMT is not possible with this logic. 
{-# INLINABLE validateUnstaking #-}
validateUnstaking :: ScriptParams -> WmtStaking -> ScriptContext -> Bool
validateUnstaking sp d@WmtStaking{..} ctx
    |   traceIfFalse "owner signature missing" $ txSignedBy' (txInfoSignatories info) swmtOwner
      , traceIfFalse "script output" $ noScriptOutputs ctx
      , traceIfFalse "execution proof error" $ proofOfExecutionBurnt sExPrCs sExprTn $ txInfoMint info
      , traceIfFalse "wrong tokens" $ onlyWMTandProofTokenOnUTxO sp d ctx = True
    | otherwise = False
    where
      info = scriptContextTxInfo ctx

{-# INLINABLE proofOfExecutionBurnt #-}
proofOfExecutionBurnt ::CurrencySymbol -> TokenName -> Value -> Bool
proofOfExecutionBurnt cs tn v = Value.valueOf v cs tn == -1

-- We must ensure only one ScriptUTxO per transaction exists.
{-# INLINABLE onlyOneScriptUTxO #-}
onlyOneScriptUTxO :: ScriptContext -> Value
onlyOneScriptUTxO ctx =
    case findOwnInput ctx of
      Just i -> txOutValue $ txInInfoResolved i
      Nothing -> traceError "No Script Inputs detected"

-- We must ensure that on the ScriptUTxO is only WMT and another token (execution proof) with the amount 1 is present
{-# INLINABLE onlyWMTandProofTokenOnUTxO #-}
onlyWMTandProofTokenOnUTxO :: ScriptParams -> WmtStaking -> ScriptContext -> Bool
onlyWMTandProofTokenOnUTxO ScriptParams{..} WmtStaking{..} ctx =
  let
    v = onlyOneScriptUTxO ctx
  in
    traceIfFalse "cannot find execution proof NFT" (Value.valueOf v sExPrCs sExprTn == 1) && traceIfFalse "no WMT on staking UTxO" (Value.valueOf v pStCs pStTn >= 1)

-- tx signed by
{-# INLINABLE txSignedBy' #-}
txSignedBy' :: [PubKeyHash] -> PubKeyHash -> Bool
txSignedBy' txInfoSignatories k =
  isJust $ find (k == ) txInfoSignatories

-- make sure no UTxOs are paid to this smart contract address
{-# INLINABLE noScriptOutputs #-}
noScriptOutputs :: ScriptContext -> Bool
noScriptOutputs ctx =
    case getContinuingOutputs ctx of
      [] -> True
      _  -> traceError "Continuing output is not allowed"


-- Validator
{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> WmtStaking -> Action -> ScriptContext -> Bool
mkVal sp d UnStake ctx = validateUnstaking sp d ctx
mkVal sp _ Admin ctx = validateAdmin sp ctx

{-# INLINABLE vUt #-}
vUt :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
vUt s d r c =
   wVal mkVal (unsafeFromBuiltinData s) (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

{-# INLINABLE wVal #-}
wVal :: forall s d r c
    . (UnsafeFromData s, UnsafeFromData d, UnsafeFromData r, UnsafeFromData c)
    => (s -> d -> r -> c -> Bool)
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wVal f s d r c = check (f (unsafeFromBuiltinData s) (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c))
