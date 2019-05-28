{-|
Internal implementation of Pencil's main functionality.
-}
module Pencil.App.Internal where

import Pencil.Config

import Control.Monad.Except
import Control.Monad.Reader (ReaderT(..))
import Data.Typeable (Typeable)
import GHC.IO.Exception (IOException(ioe_description, ioe_filename, ioe_type), IOErrorType(NoSuchThing))

import qualified Data.Text as T

-- | The primary monad transformer stack for a Pencil application.
--
-- This unrolls to:
--
-- > PencilApp a = Config -> IO (Except PencilException a)
--
-- The @ExceptT@ monad allows us to catch "checked" exceptions; errors that we
-- know how to handle, in PencilException. Note that Unknown "unchecked"
-- exceptions can still go through IO.
--
type PencilApp = ReaderT Config (ExceptT PencilException IO)

-- | Known Pencil errors that we know how to either recover from or quit
-- gracefully.
data PencilException
  = NotTextFile (Maybe FilePath)
  -- ^ Failed to read a file as a text file.
  | FileNotFound (Maybe FilePath)
  -- ^ File not found. We may or may not know the file we were looking for.
  | CollectionNotLastInStructure T.Text
  -- ^ The collection in the structure was not the last element in the
  -- structure.
  | CollectionFirstInStructure T.Text
  -- ^ A collection cannot be the first element in the structure (it's useless
  -- there, as nothing can reference the pages in the collection).
  deriving (Typeable, Show)

-- | Converts the IOError to a known 'PencilException'.
--
-- How to test errors:
--
-- @
-- import Control.Exception
-- import qualified Data.Text.IO as TIO
--
-- (\e -> print ('GHC.IO.ioe_description' (e :: IOError)) >> return "") 'Control.Exception.handle' (TIO.readFile "foo")
-- @
--
toPencilException :: IOError -> Maybe PencilException
toPencilException e
  | isInvalidByteSequence e = Just (NotTextFile (ioe_filename e))
  | isNoSuchFile e = Just (FileNotFound (ioe_filename e))
  | otherwise = Nothing

-- | Returns true if the IOError is an invalid byte sequence error. This
-- suggests that the file is a binary file.
isInvalidByteSequence :: IOError -> Bool
isInvalidByteSequence e = ioe_description e == "invalid byte sequence"

-- | Returns true if the IOError is due to missing file.
isNoSuchFile :: IOError -> Bool
isNoSuchFile e = ioe_type e == NoSuchThing

-- | Print the list of Strings, one line at a time, prefixed with "-".
printAsList :: [String] -> IO ()
printAsList [] = return ()
printAsList (a:as) = do
  putStr "- "
  putStrLn a
  printAsList as