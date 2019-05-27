{-|
Internal implementation of Pencil's main functionality.
-}
module Pencil.App.Internal where


import Pencil.Config

import Control.Monad.Except
import Control.Monad.Reader
import Data.Typeable (Typeable)

import qualified Data.Text as T

-- | The main monad transformer stack for a Pencil application.
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

-- | Print the list of Strings, one line at a time, prefixed with "-".
printAsList :: [String] -> IO ()
printAsList [] = return ()
printAsList (a:as) = do
  putStr "- "
  putStrLn a
  printAsList as
