module Pencil.Content.Nodes where

import Pencil.App.Internal
import Pencil.Env.Internal
import Pencil.Parser.Internal
import Pencil.Content.Files
import Pencil.Config

import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception (tryJust)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Text.Pandoc as P
import qualified Text.Sass as Sass
import qualified Data.Text.IO as TIO

-- | Evaluate the nodes in the given environment. Note that it returns an IO
-- because of @${partial(..)}@ calls that requires us to load a file.
evalNodes :: Env -> [PNode] -> PencilApp [PNode]
evalNodes _ [] = return []
evalNodes env (PVar var : rest) = do
  nodes <- evalNodes env rest
  case H.lookup var env of
    Nothing ->
      -- Can't find var in env. Skip over it for now and we'll just render the
      -- directive to help user debug missing variables. Later on we'll do some
      -- nice error handling w/o crashing the system (throw warnings instead of
      -- errors).
      return $ PVar var : nodes
    Just envData -> do
      displayValue <- asks getDisplayValue
      return $ PText (displayValue envData) : nodes
evalNodes env (PIf var nodes : rest) = do
  rest' <- evalNodes env rest
  case H.lookup var env of
    Nothing ->
      -- Can't find var in env; everything inside the if-statement is thrown away
      return rest'
    Just _ -> do
      -- Render nodes inside the if-statement
      nodes' <- evalNodes env nodes
      return $ nodes' ++ rest'
evalNodes env (PFor var nodes : rest) = do
  rest' <- evalNodes env rest
  case H.lookup var env of
    Nothing ->
      -- Can't find var in env; everything inside the for-statement is throw away
      return rest'
    Just (VEnvList envs) -> do
      -- Render the for nodes once for each given env, and append them together
      forNodes <-
        foldM
          (\accNodes e -> do
              nodes' <- evalNodes (H.union e env) nodes
              return $ accNodes ++ nodes')
          [] envs
      return $ forNodes ++ rest'
    -- Var is not an VEnvList; everything inside the for-statement is thrown away
    Just _ -> return rest'
evalNodes env (PPartial fp : rest) = do
  (_, nodes) <- parseAndConvertTextFiles (T.unpack fp)
  nodes' <- evalNodes env nodes
  rest' <- evalNodes env rest
  return $ nodes' ++ rest'
evalNodes env (n : rest) = do
  rest' <- evalNodes env rest
  return $ n : rest'

-- | Loads and parses the given file path. Converts 'Markdown' files to HTML,
-- compiles 'Sass' files into CSS, and leaves everything else alone.
parseAndConvertTextFiles :: FilePath -> PencilApp (T.Text, [PNode])
parseAndConvertTextFiles fp = do
  content <- loadTextFile fp
  content' <-
    case fileType fp of
      Markdown -> do
        pandocReaderOptions <- asks getPandocReaderOptions
        pandocWriterOptions <- asks getPandocWriterOptions
        result <- liftIO $ P.runIO (readMarkdownWriteHtml pandocReaderOptions pandocWriterOptions content)
        case result of
          Left _ -> return content
          Right text -> return text
      Sass -> do
        sassOptions <- asks getSassOptions
        sitePrefix <- asks getSourceDir
        -- Use compileFile so that SASS @import works
        result <- liftIO $ Sass.compileFile (sitePrefix ++ fp) sassOptions
        case result of
          Left _ -> return content
          Right byteStr -> return $ decodeUtf8 byteStr
      _ -> return content
  let nodes = case parseText content' of
                Left _ -> []
                Right n -> n
  return (content', nodes)

-- | Loads the given file as a text file. Throws an exception into the ExceptT
-- monad transformer if the file is not a text file.
loadTextFile :: FilePath -> PencilApp T.Text
loadTextFile fp = do
  sitePrefix <- asks getSourceDir
  -- Try to read the file. If it fails because it's not a text file, capture the
  -- exception and convert it to a "checked" exception in the ExceptT stack via
  -- 'throwError'.
  eitherContent <- liftIO $ tryJust toPencilException (TIO.readFile (sitePrefix ++ fp))
  case eitherContent of
    Left e -> throwError e
    Right a -> return a

-- | Converts Markdown to HTML using the given options.
readMarkdownWriteHtml :: P.PandocMonad m => P.ReaderOptions -> P.WriterOptions -> T.Text -> m T.Text
readMarkdownWriteHtml readerOptions writerOptions content = do
  pandoc <- P.readMarkdown readerOptions content
  P.writeHtml5String writerOptions pandoc