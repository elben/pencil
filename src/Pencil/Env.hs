{-|
Functions that manipulate the environment.
-}
module Pencil.Env
  ( Value(..)
  , withEnv
  , insert
  , insertText
  -- , insertPages
  , adjust
  , merge
  , toText
  , toTextRss

  , arrayContainsText
  , dateOrdering
  , maybeOrdering
  ) where

import Pencil.Env.Internal
import Pencil.App.Internal
import Pencil.Config

import qualified Control.Monad.Reader as Reader
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

-- | Inserts @Value@ into the given @Env@.
insert :: T.Text
          -- ^ Environment variable name.
          -> Value
          -- ^ @Value@ to insert.
          -> Env
          -- ^ Environment to modify.
          -> Env
insert = H.insert

-- | Modifies a variable in the given environment.
adjust :: (Value -> Value)
       -> T.Text
       -- ^ Environment variable name.
       -> Env
       -> Env
adjust = H.adjust

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

-- | Runs the computation with the given environment. This is useful when you
-- want to render a 'Pencil.Content.Internal.Page' or 'Pencil.Content.Internal.Structure' with a modified environment.
--
-- @
-- withEnv ('Pencil.Env.insertText' "newvar" "newval" env) ('Pencil.Content.render' page)
-- @
--
-- Alternatively, use 'Reader.local', which is re-exported in the Pencil module.
--
withEnv :: Env -> PencilApp a -> PencilApp a
withEnv env = Reader.local (setEnv env)

-- | Returns true if the given @Value@ is a @VArray@ that contains the given
-- text.
arrayContainsText :: T.Text -> Value -> Bool
arrayContainsText t (VArray arr) =
  any (\d -> case d of
               VText t' -> t == t'
               _ -> False)
      arr
arrayContainsText _ _ = False

-- | Defines an ordering for possibly-missing Value. Nothings are ordered last.
maybeOrdering :: (Value -> Value -> Ordering)
              -> Maybe Value -> Maybe Value -> Ordering
maybeOrdering _ Nothing Nothing = EQ
maybeOrdering _ (Just _) Nothing = GT
maybeOrdering _ Nothing (Just _) = LT
maybeOrdering o (Just a) (Just b) = o a b

-- | Sort by newest first.
dateOrdering :: Value -> Value -> Ordering
dateOrdering (VDateTime a) (VDateTime b) = compare b a
dateOrdering _ _ = EQ
