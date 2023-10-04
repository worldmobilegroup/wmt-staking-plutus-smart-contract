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

module Val.OnChain
  ( vUt
  ) where
-- import           Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Value    as Value (flattenValue,valueOf)
import           Plutus.V2.Ledger.Contexts as V2 (findOwnInput,getContinuingOutputs)
import           PlutusTx.Prelude          
import           Plutus.V2.Ledger.Api

import           Val.Types

-- Validate Admin
-- It is generally possible to deadlock if someone tries to stake with a different minitng policy as execution proof or using another NFT as execution proof.
-- This is a missuse which cannot lead to valid staking transaction but we might want to help those users.
{-# INLINABLE validateAdmin #-}
validateAdmin :: ScriptParams -> ScriptContext -> Bool
validateAdmin ScriptParams{..} ctx
    | txSignedBy' (txInfoSignatories info) pAdm = True
    | otherwise = False
    where
        info = scriptContextTxInfo ctx

-- Validate Unstaking
-- It is only possible to unstake a full UTxO, a partial unstake of WMT is not possible with this logik. 
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

-- Wir haben hier ein Problem, wir können die Minting Policy nicht kreuz-verdrahten mit diesem Smart Contract 
-- (CurrencySymbol der Policy als Parameter hier und ValidatorHash als Parameter in der Minting Policy)
-- Generell wollen wir sicherstellen das der ExecutionProof nicht wiederverwendet werden kann, dazu muss er bei Unstaking zerstört werden.
-- Da wir das CurrencySymbol nicht als parameter ablegen können, bleibt uns nur es im Datum zu speichern. 
-- Das heißt der im Datum gepseicherte Token muss zerstört werden um "unstaken" zu können. 
-- Dies erzwingt nicht das der richtige Token zerstört wird, jemand kann einen anderen Token zum staking im staking UTxO verwenden und diesen im Datum angeben,
-- Der Token hat aber die falsche policyID und der Tokenname muss nach dem richtigen Muster gebut worden sein. 
-- Für die Rewardberechnung werden aber nur Tokens mit einem bestimmten CurrencySymbol berücksichtigt, daher würde ein solch falsches UTxO nicht zur
-- Reward berechnung herangezogen werden. 
-- Eine Schwierigkeit ergibt sich dennoch: Es muss sichergestellt sein das falls zwei Tokens auf dem UTxO liegen das richtige Token zerstört wird oder das 
-- von vorneherein es verboten ist mit zwei proof Tokens (neben WMT) zu staken. Ein versehentlich so erstelles Staking-UTxO müsste durch eine Administrator transaktion 
-- ausgegeben werden.
{-# INLINABLE proofOfExecutionBurnt #-}
proofOfExecutionBurnt ::CurrencySymbol -> TokenName -> Value -> Bool
proofOfExecutionBurnt cs tn v = Value.valueOf v cs tn == -1

-- Wir müssen sicherstellen das nur ein einziges ScriptUTxO in der transaction vorhanden ist.
{-# INLINABLE onlyOneScriptUTxO #-}
onlyOneScriptUTxO :: ScriptContext -> Value
onlyOneScriptUTxO ctx = 
    case findOwnInput ctx of 
      Just i -> txOutValue $ txInInfoResolved i
      Nothing -> traceError "No Script Inputs detected" 


-- Wir müssen sicherstellen das auf dem ScriptUTxO nur WMT und ein weiter Token (der Execution Proof Token) in der Menge 1 vorhanden ist.
{-# INLINABLE onlyWMTandProofTokenOnUTxO #-}
onlyWMTandProofTokenOnUTxO :: ScriptParams -> WmtStaking -> ScriptContext -> Bool
onlyWMTandProofTokenOnUTxO ScriptParams{..} WmtStaking{..} ctx = 
  let 
    v = onlyOneScriptUTxO ctx
  in 
    traceIfFalse "cannot find execution proof NFT" ((Value.valueOf v sExPrCs sExprTn) == 1) && traceIfFalse "no WMT on staking UTxO" ((Value.valueOf v pStCs pStTn) >= 1)

-- custom tx signed by
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


-- Main Validator

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
