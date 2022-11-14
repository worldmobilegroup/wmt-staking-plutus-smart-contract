{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2022
  Version  : v0.1
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# options_ghc -fno-specialise         #-}

module OffChain
  where
import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as LB
import qualified Data.ByteString.Short          as SBS
import qualified Ledger
import           OnChain                        (vUt)
import qualified Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Address       as Address
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude
import           Types                          (ScriptParams (..))


validator :: BuiltinData -> PlutusV2.Validator
validator sp = PlutusV2.mkValidatorScript
        ($$(PlutusTx.compile [|| vUt ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sp)

escrowScript :: BuiltinData -> PlutusV2.Script
escrowScript = PlutusV2.unValidatorScript . validator

escrowHash :: BuiltinData -> PlutusV2.ValidatorHash
escrowHash = Utils.validatorHash . validator

escrowAddress :: ScriptParams -> Ledger.Address
escrowAddress sp = Address.scriptHashAddress $ escrowHash $ PlutusTx.toBuiltinData sp

scriptAsCbor :: BuiltinData -> LB.ByteString
scriptAsCbor = serialise . escrowScript

apiScript :: ScriptParams -> PlutusScript PlutusScriptV2
apiScript sp = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor (PlutusTx.toBuiltinData sp)

