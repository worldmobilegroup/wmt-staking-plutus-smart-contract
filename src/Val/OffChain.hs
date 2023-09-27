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

module Val.OffChain
  where
import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as LB
import qualified Data.ByteString.Short          as SBS
import qualified Ledger
import           Val.OnChain                        (vUt)
import qualified Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V1.Ledger.Address       as Address
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude
import           Val.Types                          (ScriptParams (..))


validator :: BuiltinData -> PlutusV2.Validator
validator sp = PlutusV2.mkValidatorScript
        ($$(PlutusTx.compile [|| vUt ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sp)

script :: BuiltinData -> PlutusV2.Script
script = PlutusV2.unValidatorScript . validator

scriptHash :: BuiltinData -> PlutusV2.ValidatorHash
scriptHash = Utils.validatorHash . validator

scriptAddress :: ScriptParams -> Ledger.Address
scriptAddress sp = Address.scriptHashAddress $ scriptHash $ PlutusTx.toBuiltinData sp

scriptAsCbor :: BuiltinData -> LB.ByteString
scriptAsCbor = serialise . script

apiScript :: ScriptParams -> PlutusScript PlutusScriptV2
apiScript sp = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor (PlutusTx.toBuiltinData sp)

------------------------------------------------------------
-- Endpoints for Testing
------------------------------------------------------------
{--
import Cardano.Node.Emulator.Params (testnet)
import Control.Lens ((^?))
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Ledger (CardanoAddress, DecoratedTxOut, datumInDatumFromQuery, decoratedTxOutPlutusValue,
               decoratedTxOutScriptDatum)
import Ledger.Tx.Constraints as Constraints hiding (adjustUnbalancedTx)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Contract
import Plutus.Contract.Test.Coverage.Analysis
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.Uniswap.OnChain (mkUniswapValidator, validateLiquidityMinting)
import Plutus.Contracts.Uniswap.Pool
import Plutus.Contracts.Uniswap.Types
import Plutus.Script.Utils.V2.Address (mkValidatorCardanoAddress)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.V2.Ledger.Api (CurrencySymbol, Datum (Datum), DatumHash, MintingPolicy, Redeemer (Redeemer), TokenName,
                             TxOutRef, Validator, Value)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Coverage
import PlutusTx.Prelude hiding (Semigroup (..), dropWhile, flip, unless)
import Prelude as Haskell (Int, Semigroup (..), Show, String, div, dropWhile, flip, show, (^))
import Text.Printf (printf)

data Wmtstaking
instance Scripts.ValidatorTypes Wmtstaking where
    type instance RedeemerType Wmtstaking = Action
    type instance DatumType    Wmtstaking = WmtStaking

type UniswapOwnerSchema = Endpoint "start" ()

-- | Schema for the endpoints for users of Uniswap.
type UniswapUserSchema =
        Endpoint "register" WmtStaking
        .\/ Endpoint "unregister"   WmtStaking
        .\/ Endpoint "stop"   ()

data ContractState = 
    Registered
  | Unregistered

--}