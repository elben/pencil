module Pencil.App
  ( PencilApp
  , PencilException(..)
  , run
  , listDir
  ) where

import Pencil.App.Internal
import Pencil.Config
import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Directory as D
import qualified System.FilePath as FP
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

-- | Lists files in given directory. The file paths returned is prefixed with the
-- given directory.
listDir :: Bool
        -- ^ Recursive if @True@.
        -> FilePath
        -> PencilApp [FilePath]
listDir recursive dir = do
  let dir' = if null dir then dir else FP.addTrailingPathSeparator dir
  fps <- listDir_ recursive dir'
  return $ map (dir' ++) fps

-- | 'listDir' helper.
listDir_ :: Bool -> FilePath -> PencilApp [FilePath]
listDir_ recursive dir = do
  sitePrefix <- asks getSourceDir
  -- List files (just the filename, without the fp directory prefix)
  listing <- liftIO $ D.listDirectory (sitePrefix ++ dir)
  -- Filter only for files (we have to add the right directory prefixes to the
  -- file check)
  files <- liftIO $ filterM (\f -> D.doesFileExist (sitePrefix ++ dir ++ f)) listing
  dirs <- liftIO $ filterM (\f -> D.doesDirectoryExist (sitePrefix ++ dir ++ f)) listing

  innerFiles <- if recursive
                  then mapM
                         (\d -> do
                           ff <- listDir_ recursive (dir ++ d ++ "/")
                           -- Add the inner directory as a prefix
                           return (map (\f -> d ++ "/" ++ f) ff))
                         dirs
                  else return []

  return $ files ++ concat innerFiles

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