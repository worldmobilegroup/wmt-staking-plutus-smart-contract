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
import           Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Value         as Value
import           Plutus.V2.Ledger.Api           (Credential (PubKeyCredential, ScriptCredential),
                                                 CurrencySymbol, PubKeyHash,
                                                 TokenName, UnsafeFromData,
                                                 Value, unsafeFromBuiltinData)
import           PlutusTx.Prelude

import           Types


{-# INLINABLE validateRegister #-}
validateRegister :: ScriptParams -> EnRegistration -> ATxInfo -> Bool
validateRegister ScriptParams{..} EnRegistration{..} info
    | isEnNftSend, isNftPaidByOwner, txSignedBy' (atxInfoSignatories info) enOwner = True
    | otherwise = False
    where

        -- only exactly one EnNFT can be used for registration, the value has to be paid to the script
        isEnNftSend :: Bool
        isEnNftSend = Value.valueOf (valueLockedBy' info $ ownHash' $ atxInfoInputs info ) pNftCs enUsedNftTn == 1

        -- the NFT has to be sent by the owner specified in the datum
        isNftPaidByOwner :: Bool
        isNftPaidByOwner = sendfromOwner (atxInfoOutputs info) enOwner pNftCs enUsedNftTn




{-# INLINABLE validateUnregister #-}
validateUnregister :: ScriptParams -> EnRegistration -> ATxInfo -> Bool
validateUnregister ScriptParams{..} EnRegistration{..} info
    | txSignedBy' (atxInfoSignatories info) enOwner, isEnNftSpent, noOutputsToScript = True
    | otherwise = False
    where
      -- is only the NFT specified in script params and datum spent back to the owner
      isEnNftSpent :: Bool
      isEnNftSpent = Value.valueOf (valuePaidTo' info enOwner ) pNftCs enUsedNftTn == 1

      -- no ouput is sent to the script address
      noOutputsToScript :: Bool
      noOutputsToScript =
          let scriptValues :: [Value]
              scriptValues = scriptOutputsAt' (ownHash' $ atxInfoInputs info) info
          in
            case scriptValues of
              [] -> True
              _  -> False


{-# INLINABLE validateAdmin #-}
validateAdmin :: ScriptParams -> ATxInfo -> Bool
validateAdmin ScriptParams{..} info
    -- is signed by admin wallet
    | txSignedBy' (atxInfoSignatories info) adm = True
    | otherwise = False


{-# INLINABLE isOwnerAddr #-}
isOwnerAddr :: AAddress -> PubKeyHash -> Bool
isOwnerAddr AAddress { aaddressCredential } pkh = case aaddressCredential of
  PubKeyCredential c -> c == pkh
  _                  -> False

{-# INLINABLE sendfromOwner #-}
sendfromOwner :: [ATxOut] -> PubKeyHash -> CurrencySymbol -> TokenName -> Bool
sendfromOwner i h c t =
  let
    isfromOwner :: ATxOut -> Bool
    isfromOwner a = isOwnerAddr (atxOutAddress a) h
  in case filter isfromOwner i of
    [ATxOut{atxOutValue=v}] -> Value.valueOf v c t == 1
    _                       ->  False

{-# INLINABLE txSignedBy' #-}
txSignedBy' :: [PubKeyHash] -> PubKeyHash -> Bool
txSignedBy' atxInfoSignatories k =
  isJust $ find (k == ) atxInfoSignatories

{-# INLINABLE pubKeyOutputsAt' #-}
pubKeyOutputsAt' :: PubKeyHash -> ATxInfo -> [Value]
pubKeyOutputsAt' pk p =
    let flt ATxOut{ atxOutAddress = AAddress (PubKeyCredential pk') _, atxOutValue } | pk == pk' = Just atxOutValue
                                                                                     | otherwise = Nothing
        flt _                     = Nothing
    in mapMaybe flt (atxInfoOutputs p)

{-# INLINABLE valuePaidTo' #-}
valuePaidTo' :: ATxInfo -> PubKeyHash -> Value
valuePaidTo' info pkh = mconcat (pubKeyOutputsAt' pkh info)

{-# INLINABLE valueLockedBy' #-}
valueLockedBy' :: ATxInfo -> ValidatorHash -> Value
valueLockedBy' info h =
    let outputs = scriptOutputsAt' h info
    in mconcat outputs

{-# INLINABLE scriptOutputsAt' #-}
scriptOutputsAt' :: ValidatorHash -> ATxInfo -> [Value]
scriptOutputsAt' h p =
    let flt ATxOut{atxOutAddress=AAddress (ScriptCredential s) _, atxOutValue} | s == h = Just atxOutValue
        flt _ = Nothing
    in mapMaybe flt (atxInfoOutputs p)


{-# INLINABLE ownHash' #-}
ownHash' :: [ATxInInfo] -> ValidatorHash
ownHash' l =
  let
    checkInput :: ATxInInfo -> Bool
    checkInput ATxInInfo{atxInInfoResolved=ATxOut{atxOutAddress=AAddress (ScriptCredential _) _}} = True
    checkInput _ = False
  in case filter checkInput l of
    [ATxInInfo{atxInInfoResolved=ATxOut{atxOutAddress=AAddress (ScriptCredential s) _}}] -> s
    _                          -> error ()

{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> EnRegistration -> Action -> AScriptContext -> Bool
mkVal sp d Register   ctx = validateRegister   sp d $ aScriptContextTxInfo ctx
mkVal sp d Unregister ctx = validateUnregister sp d $ aScriptContextTxInfo ctx
mkVal sp _ Admin      ctx = validateAdmin      sp $ aScriptContextTxInfo ctx

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
