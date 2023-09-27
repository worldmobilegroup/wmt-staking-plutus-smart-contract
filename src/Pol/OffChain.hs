{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v0.1
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# options_ghc -fno-specialise         #-}

module Pol.OffChain
  where
import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as LB
import qualified Data.ByteString.Short          as SBS
import           Pol.OnChain                        (vUt)
import qualified Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude
import           Pol.Types                          (ScriptParams (..))
import qualified Plutus.V1.Ledger.Value  as Value

mp :: BuiltinData -> PlutusV2.MintingPolicy
mp sp = PlutusV2.mkMintingPolicyScript
       ($$(PlutusTx.compile [|| vUt ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sp)

script :: BuiltinData -> PlutusV2.Script
script = PlutusV2.getMintingPolicy . mp

scriptHash :: BuiltinData -> PlutusV2.MintingPolicyHash
scriptHash = Utils.mintingPolicyHash . mp

mustMintPolicyCurrencySymbol :: BuiltinData -> Value.CurrencySymbol
mustMintPolicyCurrencySymbol sp = Value.mpsSymbol (scriptHash sp)

scriptAsCbor :: BuiltinData -> LB.ByteString
scriptAsCbor = serialise . script

apiScript :: ScriptParams -> PlutusScript PlutusScriptV2
apiScript sp = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor (PlutusTx.toBuiltinData sp)

------------------------------------------------------------
-- Endpoints for Testing
------------------------------------------------------------

