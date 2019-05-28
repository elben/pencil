{-# LANGUAGE OverloadedStrings #-}

{-|
Internal implementation of Pencil's environment.
-}
module Pencil.Env.Internal where

import qualified Pencil.Parser as P

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

-- | Converts an Aeson 'Aeson.Value' to a Pencil 'Value'.
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

-- | Accepted format is ISO 8601 (YYYY-MM-DD), optionally with an appended "THH:MM:SS".
-- Examples:
--
-- * 2010-01-30
-- * 2010-01-30T09:08:00
--
toDateTime :: String -> Maybe TC.UTCTime
toDateTime s =
  -- Try to parse "YYYY-MM-DD"
  case parseIso8601 Nothing s of
    -- Try to parse "YYYY-MM-DDTHH:MM:SS"
    Nothing -> parseIso8601 (Just "%H:%M:%S") s
    Just dt -> Just dt

-- | Helper for 'TF.parseTimeM' using ISO 8601. YYYY-MM-DDTHH:MM:SS and
-- YYYY-MM-DD formats.
--
-- https://hackage.haskell.org/package/time-1.9/docs/Data-Time-Format.html#v:iso8601DateFormat
parseIso8601 :: Maybe String -> String -> Maybe TC.UTCTime
parseIso8601 f = TF.parseTimeM True TF.defaultTimeLocale (TF.iso8601DateFormat f)

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

-- | Gets the nodes from the env, from the @this.nodes@ variable. Returns empty
-- list if this variable is missing.
getNodes :: Env -> [P.PNode]
getNodes env =
  case H.lookup "this.nodes" env of
    Just (VNodes nodes) -> nodes
    _ -> []

-- | Gets the rendered content from the env, from the @this.content@ variable.
-- Returns empty is the variable is missing (e.g. has not been rendered).
getContent :: Env -> Maybe T.Text
getContent env =
  case H.lookup "this.content" env of
    Just (VText content) -> return content
    _ -> Nothing

-- | Renders environment value for human consumption. This is the default one.
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

-- | A version of 'toText' that renders 'Value' acceptable for an RSS feed.
--
-- * Dates are rendered in the RFC 822 format.
-- * Everything else defaults to the 'toText' implementation.
--
-- You'll probably want to also use 'Pencil.Content.Internal.escapeXml' to render an RSS feed.
--
toTextRss :: Value -> T.Text
toTextRss (VDateTime dt) = T.pack $ TF.formatTime TF.defaultTimeLocale rfc822DateFormat dt
toTextRss v = toText v

-- | RFC 822 date format.
--
-- Helps to pass https://validator.w3.org/feed/check.cgi.
--
-- Same as https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:rfc822DateFormat
-- but no padding for the day section, so that single-digit days only has one space preceeding it.
--
-- Also changed to spit out the offset timezone (+0000) because the default was spitting out "UTC"
-- which is not valid RFC 822. Weird, since the defaultTimeLocal source and docs show that it won't
-- use "UTC":
-- https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:defaultTimeLocale
--
rfc822DateFormat :: String
rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %z"