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
--import           Cardano.Api.Shelley            (PlutusScriptV2)
--import           Codec.Serialise                (serialise)
--import           Ledger.Ada                     as Ada
import           Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Value         as Value
import           Plutus.V2.Ledger.Api           (Credential (PubKeyCredential, ScriptCredential),
                                                 CurrencySymbol, PubKeyHash,
                                                 TokenName, UnsafeFromData,
                                                 Value, unsafeFromBuiltinData)
--import qualified Plutus.V2.Ledger.Api           as PlutusV2
--import qualified PlutusTx
import           PlutusTx.Prelude

import           Types

{-
Was muss sichergestellt sein während der Registrierung?
 - NFT soll gelockt werden
 - NFT muss zu einem bestimmten CS gehören
 - der NFT muss von der Addresse des enOwner kommen
 - Das Datum soll enthalten:
    - ENO Validator ID
    - ConfigHash
    - TokenName
    - OwnerPubKeyHash

-}

{-# INLINABLE validateRegister #-}
validateRegister :: ScriptParams -> EnRegistration -> ATxInfo -> Bool
validateRegister ScriptParams{..} EnRegistration{..} info
    | isEnNftSend, txSignedBy' (atxInfoSignatories info) enOwner, isNftPaidByOwner = True
    | otherwise = False
    where

        --isPaid :: PubKeyHash -> Integer -> Bool
        --isPaid addr amt = Ada.fromValue (valuePaidTo' info addr) >= Ada.lovelaceOf amt

        -- only exactly one EnNFT can be user for registration, the value has to be paid to the script
        isEnNftSend :: Bool
        isEnNftSend = Value.valueOf (valueLockedBy' info $ ownHash' $ atxInfoInputs info ) pNftCs enUsedNftTn == 1

        -- der NFT muss von der Addresse des enOwner kommen
        isNftPaidByOwner :: Bool
        isNftPaidByOwner = True
        



{-# INLINABLE validateUnregister #-}
validateUnregister :: ScriptParams -> EnRegistration -> ATxInfo -> Bool
validateUnregister ScriptParams{..} EnRegistration{..} info
    | txSignedBy' (atxInfoSignatories info) enOwner, isEnNftSpent, noOutputsToScript = True
    | otherwise = False
    where
      isEnNftSpent :: Bool
      isEnNftSpent = Value.valueOf (valuePaidTo' info enOwner ) pNftCs enUsedNftTn == 1

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
    | txSignedBy' (atxInfoSignatories info) adm = True
    | otherwise = False

--{-# INLINABLE minAda' #-}
--minAda' :: Integer
--minAda' = 1000000

--{-# INLINABLE cf #-}
--cf :: Integer -> Integer -> Integer
--cf i j = PlutusTx.Prelude.max minAda' (i `PlutusTx.Prelude.divide` 1000 * j)

--{-# INLINABLE isScriptAddress #-}
--isScriptAddress :: AAddress -> Bool
--isScriptAddress AAddress { aaddressCredential } = case aaddressCredential of
--  ScriptCredential _ -> True
--  _                  -> False

--{-# INLINABLE scriptOutputsOk #-}
--scriptOutputsOk :: [ATxOut] -> CurrencySymbol -> TokenName -> Bool
--scriptOutputsOk i c t =
--  let
--    isScriptOutput :: ATxOut -> Bool
--    isScriptOutput = isScriptAddress . atxOutAddress

--  in case filter isScriptOutput i of
--    [ATxOut{atxOutValue=v}] -> Value.valueOf v c t >= 1
--    _                       ->  False

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
-- | Get the list of 'TxOut' outputs of the pending transaction at
--   a given script address.
scriptOutputsAt' :: ValidatorHash -> ATxInfo -> [Value]
scriptOutputsAt' h p =
    let flt ATxOut{atxOutAddress=AAddress (ScriptCredential s) _, atxOutValue} | s == h = Just atxOutValue
        flt _ = Nothing
    in mapMaybe flt (atxInfoOutputs p)


{-# INLINABLE ownHash' #-}
-- | Get the validator hash of the output that is curently being validated
ownHash' :: [ATxInInfo] -> ValidatorHash
ownHash' l =
  let
    checkInput :: ATxInInfo -> Bool
    checkInput ATxInInfo{atxInInfoResolved=ATxOut{atxOutAddress=AAddress (ScriptCredential _) _}} = True
    checkInput _ = False
  in case filter checkInput l of
    [ATxInInfo{atxInInfoResolved=ATxOut{atxOutAddress=AAddress (ScriptCredential s) _}}] -> s
    _                          -> traceError "Lg" -- "Can't get validator hashes"
--ownHash' _ = traceError "Lg" -- "Can't get validator hashes"

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
