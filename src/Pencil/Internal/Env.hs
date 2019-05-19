{-# LANGUAGE OverloadedStrings #-}

{-|
Internal implementation of Pencil's environment.
-}
module Pencil.Internal.Env where

import qualified Pencil.Internal.Parser as P

import qualified Data.HashMap.Strict as H
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Time.Clock as TC
import qualified Data.Time.Format as TF
import qualified Data.Vector as V
import qualified Data.Yaml as A

-- | Represents the data types found in an environment.
--
-- This includes at least 'Data.Aeson' Value type
-- (<https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:Value here>),
-- plus other useful ones.
data Value =
    VNull -- JSON null
  | VText T.Text
  | VBool Bool
  | VDateTime TC.UTCTime
  | VArray [Value]
  | VEnvList [Env]
  | VNodes [P.PNode]
  deriving (Eq, Show)

-- | Environment map of variables to 'Value's.
type Env = H.HashMap T.Text Value

-- | Converts an Aeson @Value@ to a Pencil 'Value'.
toValue :: A.Value -> Maybe Value
toValue A.Null = Just VNull
toValue (A.Bool b) = Just $ VBool b
toValue (A.String s) =
  -- See if coercible to datetime
  case toDateTime (T.unpack s) of
    Nothing -> Just $ VText s
    Just dt -> Just $ VDateTime dt
toValue (A.Array arr) =
  Just $ VArray (V.toList (V.mapMaybe toValue arr))
toValue _ = Nothing

-- | Render for human consumption. This is the default one. Pass into Config as
-- part of the Reader?
toText :: Value -> T.Text
toText VNull = "null"
toText (VText t) = t
toText (VArray arr) = T.unwords $ map toText arr
toText (VBool b) = if b then "true" else "false"
toText (VEnvList envs) = T.unwords $ map (T.unwords . map toText . H.elems) envs
toText (VDateTime dt) =
  -- December 30, 2017
  T.pack $ TF.formatTime TF.defaultTimeLocale "%B %e, %Y" dt
toText (VNodes nodes) = P.renderNodes nodes

-- | Accepted format is ISO 8601 (YYYY-MM-DD), optionally with an appended "THH:MM:SS".
-- Example: 2010-01-30, 2010-01-30T09:08:00
toDateTime :: String -> Maybe TC.UTCTime
toDateTime s =
  -- Try to parse "YYYY-MM-DD"
  case maybeParseIso8601 Nothing s of
    -- Try to parse "YYYY-MM-DDTHH:MM:SS"
    Nothing -> maybeParseIso8601 (Just "%H:%M:%S") s
    Just dt -> Just dt

-- | Helper for 'TF.parseTimeM' using ISO 8601. YYYY-MM-DDTHH:MM:SS and
-- YYYY-MM-DD formats.
--
-- https://hackage.haskell.org/package/time-1.9/docs/Data-Time-Format.html#v:iso8601DateFormat
maybeParseIso8601 :: Maybe String -> String -> Maybe TC.UTCTime
maybeParseIso8601 f = TF.parseTimeM True TF.defaultTimeLocale (TF.iso8601DateFormat f)

-- | Define an ordering for possibly-missing Value. Nothings are ordered last.
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

-- | Returns true if the given @Value@ is a @VArray@ that contains the given
-- string.
arrayContainsString :: T.Text -> Value -> Bool
arrayContainsString t (VArray arr) =
  any (\d -> case d of
               VText t' -> t == t'
               _ -> False)
      arr
arrayContainsString _ _ = False

-- | Get the nodes from the env, from the @this.nodes@ variable. Returns empty
-- list if this variable is missing.
getNodes :: Env -> [P.PNode]
getNodes env =
  case H.lookup "this.nodes" env of
    Just (VNodes nodes) -> nodes
    _ -> []

-- | Get the render's content from the env, from the @this.content@ variable.
-- Returns Nothing is the variable is missing (e.g. has not been rendered)
getContent :: Env -> Maybe T.Text
getContent env =
  case H.lookup "this.content" env of
    Just (VText content) -> return content
    _ -> Nothing
