module Pencil.Env
  ( module Pencil.Env.Internal
  , findEnv
  , aesonToEnv
  , maybeInsertIntoEnv
  ) where

import Pencil.Env.Internal
import Pencil.Parser.Internal

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Maybe as M
import qualified Data.HashMap.Strict as H
import qualified Data.Yaml as A
import qualified Data.Text as T

-- | Find preamble node, and load as an Env. If no preamble is found, return a
-- blank Env.
findEnv :: [PNode] -> Env
findEnv nodes =
  aesonToEnv $ M.fromMaybe H.empty (findPreambleText nodes >>= (A.decodeThrow . encodeUtf8 . T.strip))

-- | Converts an Aeson Object to an Env.
aesonToEnv :: A.Object -> Env
aesonToEnv = H.foldlWithKey' maybeInsertIntoEnv H.empty

-- | Convert known Aeson types into known Env types.
maybeInsertIntoEnv :: Env -> T.Text -> A.Value -> Env
maybeInsertIntoEnv env k v =
  case toValue v of
    Nothing -> env
    Just d -> H.insert k d env
