module Pencil.Env
  ( module Pencil.Env.Internal

  , merge
  , insert
  , adjust
  , insertText

  , findEnv
  , aesonToEnv
  , maybeInsertIntoEnv
  ) where

import Pencil.Env.Internal
import Pencil.Parser.Internal
import Pencil.Content.Internal
import Pencil.App.Internal
import Pencil.Config

import Data.Text.Encoding (encodeUtf8)

import qualified Data.Maybe as M
import qualified Data.HashMap.Strict as H
import qualified Data.Yaml as A
import qualified Data.Text as T

-- | Merges two @Env@s together, biased towards the left-hand @Env@ on duplicates.
merge :: Env -> Env -> Env
merge = H.union

-- | Inserts text into the given @Env@.
--
-- @
-- env <- asks getEnv
-- insertText "title" "My Awesome Website" env
-- @
insertText :: T.Text
           -- ^ Environment variable name.
           -> T.Text
           -- ^ Text to insert.
           -> Env
           -- ^ Environment to modify.
           -> Env
insertText var val = H.insert var (VText val)

-- | Modifies a variable in the given environment.
adjust :: (Value -> Value)
       -> T.Text
       -- ^ Environment variable name.
       -> Env
       -> Env
adjust = H.adjust

-- | Inserts @Value@ into the given @Env@.
insert :: T.Text
          -- ^ Environment variable name.
          -> Value
          -- ^ @Value@ to insert.
          -> Env
          -- ^ Environment to modify.
          -> Env
insert = H.insert

-- | Find preamble node, and load as an Env. If no preamble is found, return a
-- blank Env.
findEnv :: [PNode] -> Env
findEnv nodes =
  aesonToEnv $ M.fromMaybe H.empty (findPreambleText nodes >>= (A.decodeThrow . encodeUtf8 . T.strip))

-- | Converts an Aeson Object to an Env.
aesonToEnv :: A.Object -> Env
aesonToEnv = H.foldlWithKey' maybeInsertIntoEnv H.empty

-- | Convert known Aeson 'Aeson.Value' into a Pencil
-- 'Pencil.Env.Internal.Value', and insert into the env. If there is no
-- conversion possible, the env is not modified.
maybeInsertIntoEnv :: Env -> T.Text -> A.Value -> Env
maybeInsertIntoEnv env k v =
  case toValue v of
    Nothing -> env
    Just d -> H.insert k d env

