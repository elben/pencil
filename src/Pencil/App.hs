
{-|
Pencil's run and error types.
-}
module Pencil.App
  (
  -- | Re-exports the internal module.
    module Pencil.App.Internal
  , module Pencil.App
  ) where

import Pencil.App.Internal
import Pencil.Env.Internal
import Pencil.Content
import Pencil.Config

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.EditDistance as EditDistance

-- | Run the Pencil app.
--
-- Note that this can throw a fatal exception.
run :: PencilApp a -> Config -> IO ()
run app config = do
  e <- runExceptT $ runReaderT app config
  case e of
    Left (FileNotFound (Just fp)) -> do
      e2 <- runExceptT $ runReaderT (mostSimilarFiles fp) config
      case e2 of
        Right closestFiles ->
          if not (null closestFiles)
          then do
            putStrLn ("File " ++ fp ++ " not found. Maybe you meant: ")
            printAsList (take 3 closestFiles)
          else putStrLn ("File " ++ fp ++ " not found.")
        _ -> return ()
    Left (CollectionNotLastInStructure name) ->
      putStrLn ("Collections must be last in a Structure. But the collection named " ++
               T.unpack name ++ " was not the last in the Structure.")
    Left (CollectionFirstInStructure name) ->
      putStrLn ("Collections cannot be first in a Structure. But the collection named " ++
               T.unpack name ++ " first in the Structure.")
    Left (NotTextFile fp) ->
      putStrLn ("Tried to load " ++ maybe "UNKNOWNFILE" id fp ++ " as text file, but either it's not a text file or the file is corrupted.")
    _ -> return ()

  case e of
    -- Force program to exit with error state
    Left _ -> fail "Exception when running program!"
    _ -> return ()

-- | Runs the computation with the given environment. This is useful when you
-- want to render a 'Pencil.Content.Internal.Page' or 'Pencil.Content.Internal.Structure' with a modified environment.
--
-- @
-- withEnv ('Pencil.Env.insertText' "newvar" "newval" env) ('render' page)
-- @
--
-- Alternatively, use 'Reader.local', which is re-exported in the Pencil module.
--
withEnv :: Env -> PencilApp a -> PencilApp a
withEnv env = local (setEnv env)

-- | Given a file path, look at all file paths and find the one that seems most
-- similar.
mostSimilarFiles :: FilePath -> PencilApp [FilePath]
mostSimilarFiles fp = do
  sitePrefix <- asks getSourceDir
  fps <- listDir True ""
  let fps' = map (sitePrefix ++) fps -- add site prefix for distance search
  let costs = map (\f -> (f, EditDistance.levenshteinDistance EditDistance.defaultEditCosts fp f)) fps'
  let sorted = L.sortBy (\(_, d1) (_, d2) -> compare d1 d2) costs
  return $ map fst sorted
