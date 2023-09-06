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

module OnChain
  ( vUt
  ) where
-- import           Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Value    as Value
--import           Plutus.V2.Ledger.Api           ( --, Credential (PubKeyCredential,ScriptCredential),
--                                                 PubKeyHash, UnsafeFromData,
--                                                 Value, unsafeFromBuiltinData)
import           Plutus.V2.Ledger.Contexts as V2 ()
import           PlutusTx.Prelude

import           Plutus.V2.Ledger.Api

import           Types


{-# INLINABLE validateUnregister #-}
validateUnregister :: ScriptParams -> EnRegistration -> TxInfo -> Bool
validateUnregister ScriptParams{..} EnRegistration{..} info
    | txSignedBy' (txInfoSignatories info) enOwner, noScriptOutputs $ txInfoOutputs info = True
    | otherwise = False
    where
      -- In the valuePaidTo original version is an error still needs investiagtion, at the moment the is NFT paid constraint
      -- is deactivated on testnet but the owner signature on no ScriptOutputs are used.
      isEnNftSpent :: Bool
      isEnNftSpent = Value.valueOf (valuePaidTo' info enOwner ) pNftCs enUsedNftTn == 1

{-# INLINABLE validateAdmin #-}
validateAdmin :: ScriptParams -> TxInfo -> Bool
validateAdmin ScriptParams{..} info
    | txSignedBy' (txInfoSignatories info) adm = True
    | otherwise = False


{-# INLINABLE txSignedBy' #-}
txSignedBy' :: [PubKeyHash] -> PubKeyHash -> Bool
txSignedBy' txInfoSignatories k =
  isJust $ find (k == ) txInfoSignatories

{-# INLINABLE pubKeyOutputsAt' #-}
pubKeyOutputsAt' :: PubKeyHash -> TxInfo -> [Value]
pubKeyOutputsAt' pk p =
    let flt TxOut{ txOutAddress = Address (PubKeyCredential pk') _, txOutValue } | pk == pk' = Just txOutValue
                                                                                     | otherwise = Nothing
        flt _                     = Nothing
    in mapMaybe flt (txInfoOutputs p)

{-# INLINABLE valuePaidTo' #-}
valuePaidTo' :: TxInfo -> PubKeyHash -> Value
valuePaidTo' info pkh = mconcat (pubKeyOutputsAt' pkh info)

--{-# INLINABLE valueLockedBy' #-}
--valueLockedBy' :: ATxInfo -> ValidatorHash -> Value
--valueLockedBy' info h =
--    let outputs = scriptOutputsAt' h info
--    in mconcat outputs

--{-# INLINABLE scriptOutputsAt' #-}
--scriptOutputsAt' :: ValidatorHash -> ATxInfo -> [Value]
--scriptOutputsAt' h p =
--    let flt ATxOut{atxOutAddress=AAddress (ScriptCredential s) _, atxOutValue} | s == h = Just atxOutValue
--        flt _ = Nothing
--    in mapMaybe flt (atxInfoOutputs p)


--{-# INLINABLE ownHash' #-}
--ownHash' :: [ATxInInfo] -> ValidatorHash
--ownHash' l =
--  let
--    checkInput :: ATxInInfo -> Bool
--    checkInput ATxInInfo{atxInInfoResolved=ATxOut{atxOutAddress=AAddress (ScriptCredential _) _}} = True
--    checkInput _ = False
--  in case filter checkInput l of
--     [ATxInInfo{atxInInfoResolved=ATxOut{atxOutAddress=AAddress (ScriptCredential s) _}}] -> s
--     _                          -> error () --traceError "ownHash"

{-# INLINABLE noScriptOutputs #-}
noScriptOutputs :: [TxOut] -> Bool
noScriptOutputs [] = True
noScriptOutputs (h:t) =
  let
    checkInput :: TxOut -> Bool
    checkInput TxOut{txOutAddress=Address (ScriptCredential _) _} = False
    checkInput _                                                  = True
  in
    checkInput h && noScriptOutputs t

{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> EnRegistration -> Action -> ScriptContext -> Bool
--mkVal sp d Register   ctx = validateRegister   sp d $ aScriptContextTxInfo ctx
mkVal sp d Unregister ctx = validateUnregister sp d $ scriptContextTxInfo ctx
mkVal sp _ Admin      ctx = validateAdmin      sp $ scriptContextTxInfo ctx
mkVal _ _ _ _             = False

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
