{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Pencil.Internal.Pencil where

import Pencil.Internal.Env
import Pencil.Internal.Parser

import Control.Exception (tryJust)
import Control.Monad (forM_, foldM, filterM, liftM)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Char (toLower)
import Data.Default (Default)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException(ioe_description, ioe_filename, ioe_type), IOErrorType(NoSuchThing))
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import Text.Pandoc.Extensions (enableExtension, Extension(..))
import Text.Sass.Options (defaultSassOptions)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as A
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Highlighting
import qualified Text.Pandoc.XML as XML
import qualified Text.Sass as Sass

import Debug.Trace

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

-- | The main @Config@ needed to build your website. Your app's @Config@ is
-- passed into the 'PencilApp' monad transformer.
--
-- Use 'defaultConfig' as a starting point, along with the config-modification
-- helpers such as 'setSourceDir'.
--
-- * @configRootUrl@ - URL prefix for the link attribute in RSS feeds.
--
data Config = Config
  { configSourceDir :: FilePath
  , configOutputDir :: FilePath
  , configRootUrl :: Maybe T.Text
  , configEnv :: Env
  , configDisplayValue :: Value -> T.Text
  , configSassOptions :: Sass.SassOptions
  , configPandocReaderOptions :: P.ReaderOptions
  , configPandocWriterOptions :: P.WriterOptions
  }

-- 'Data.Default.Default' instance for 'Config'.
instance Default Config where
  def = defaultConfig

-- | This default @Config@ gives you everything you need to start.
--
-- Default values:
--
-- @
-- Config
--  { 'configSourceDir' = "site/"
--  , 'configOutputDir' = "out/"
--  , 'configRootUrl' = Nothing
--  , 'configEnv' = HashMap.empty
--  , 'configDisplayValue' = 'toText'
--  , 'configSassOptions' = Text.Sass.Options.defaultSassOptions
--  , 'configPandocReaderOptions' = Text.Pandoc.def {
--       Text.Pandoc.readerExtensions = Text.Pandoc.Extensions.getDefaultExtensions "markdown"
--    }
--  , 'configPandocWriterOptions' = Text.Pandoc.def { Text.Pandoc.writerHighlightStyle = Just Text.Pandoc.Highlighting.monochrome }
--  , 'configDisplayValue = 'toText'
--  }
-- @
--
defaultConfig :: Config
defaultConfig = Config
  { configSourceDir = "site/"
  , configOutputDir = "out/"
  , configRootUrl = Nothing
  , configEnv = H.empty
  , configSassOptions = Text.Sass.Options.defaultSassOptions

  -- For markdown reader. We use getDefaultExtensions "markdown" here to get default Markdown extensions.
  -- See https://hackage.haskell.org/package/pandoc-2.5/docs/Text-Pandoc-Extensions.html#v:getDefaultExtensions
  , configPandocReaderOptions = P.def {
      P.readerExtensions = P.getDefaultExtensions "markdown"
    }
  , configPandocWriterOptions = P.def {
      P.writerHighlightStyle = Just Text.Pandoc.Highlighting.monochrome
    }
  , configDisplayValue = toText
  }

-- | The directory path of your web page source files.
getSourceDir :: Config -> FilePath
getSourceDir = configSourceDir

-- | Sets the source directory of your web page source files.
setSourceDir :: FilePath -> Config -> Config
setSourceDir fp c = c { configSourceDir = fp }

-- | The directory path of your rendered web pages.
getOutputDir :: Config -> FilePath
getOutputDir = configOutputDir

-- | Sets the output directory of your rendered web pages.
setOutputDir :: FilePath -> Config -> Config
setOutputDir fp c = c { configOutputDir = fp }

-- | The URL domain prefix.
getRootUrl :: Config -> Maybe T.Text
getRootUrl = configRootUrl

-- | Sets URL domain prefix.
setRootUrl :: T.Text -> Config -> Config
setRootUrl url c = c { configRootUrl = Just url }

-- | The environment of the @Config@, which is what the @PencilApp@ monad
-- transformer uses. This is where variables are set for rendering template
-- directives.
getEnv :: Config -> Env
getEnv = configEnv

-- | Sets the current environment. You may also want to look at 'withEnv' if you
-- want to 'render' things in a modified environment.
setEnv :: Env -> Config -> Config
setEnv env c = c { configEnv = env }

-- | Update the 'Env' inside the 'Config'.
updateEnv :: (Env -> Env) -> Config -> Config
updateEnv f c = c { configEnv = f (getEnv c) }

-- | The 'Sass.SassOptions' for rendering Sass/Scss files.
getSassOptions :: Config -> Sass.SassOptions
getSassOptions = configSassOptions

-- | Sets the 'Sass.SassOptions'.
setSassOptions :: Sass.SassOptions -> Config -> Config
setSassOptions env c = c { configSassOptions = env }

-- | The 'Text.Pandoc.ReaderOptions' for reading files that use Pandoc.
-- Supported formats by Pencil are: Markdown.
getPandocReaderOptions :: Config -> P.ReaderOptions
getPandocReaderOptions = configPandocReaderOptions

-- | Sets the 'Text.Pandoc.ReaderOptions'. For example, you may want to enable
-- some Pandoc extensions like 'Text.Pandoc.Extensions.Ext_literate_haskell':
--
-- @
-- setPandocReaderOptions
--   (Text.Pandoc.def { 'Text.Pandoc.Options.readerExtensions' = extensionsFromList [Ext_literate_haskell] })
--   config
-- @
setPandocReaderOptions :: P.ReaderOptions -> Config -> Config
setPandocReaderOptions o c = c { configPandocReaderOptions = o }

-- | The 'Text.Pandoc.WriterOptions' for rendering files that use Pandoc.
-- Supported formats by Pencil are: Markdown.
getPandocWriterOptions :: Config -> P.WriterOptions
getPandocWriterOptions = configPandocWriterOptions

-- | Sets the 'Text.Pandoc.WriterOptions'.
setPandocWriterOptions :: P.WriterOptions -> Config -> Config
setPandocWriterOptions o c = c { configPandocWriterOptions = o }

-- | The function that renders 'Value' to text.
getDisplayValue :: Config -> Value -> T.Text
getDisplayValue = configDisplayValue

-- | Sets the function that renders 'Value' to text. Overwrite this with your
-- own function if you would like to change how certain 'Value's are rendered
-- (e.g. 'VDateTime').
--
-- @
-- myRender :: Value -> T.Text
-- myRender ('VDateTime' dt) = 'T.pack' $ 'TF.formatTime' 'TF.defaultTimeLocale' "%e %B %Y" dt
-- myRender t = 'toText' t
--
-- ...
--
-- setDisplayValue myRender config
-- @
--
-- In the above example, we change the @VDateTime@ rendering to show @25
-- December 2017@. Leave everything else unchanged.
--
setDisplayValue :: (Value -> T.Text) -> Config -> Config
setDisplayValue f c = c { configDisplayValue = f }

-- | Run the Pencil app.
--
-- Note that this can throw a fatal exception.
run :: PencilApp a -> Config -> IO ()
run app config = do
  e <- runExceptT $ runReaderT app config
  case e of
    Left (VarNotInEnv var fp) ->
      putStrLn ("Variable ${" ++ T.unpack var ++ "}" ++ " not found in the environment when rendering file " ++ fp ++ ".")
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
    _ -> return ()

-- Print the list of Strings, one line at a time, prefixed with "-".
printAsList :: [String] -> IO ()
printAsList [] = return ()
printAsList (a:as) = do
  putStr "- "
  putStrLn a
  printAsList as

-- | Given a file path, look at all file paths and find the one that seems most
-- similar.
mostSimilarFiles :: FilePath -> PencilApp [FilePath]
mostSimilarFiles fp = do
  sitePrefix <- asks getSourceDir
  fps <- listDir True ""
  let fps' = map (sitePrefix ++) fps -- add site prefix for distance search
  let costs = map (\f -> (f, levenshteinDistance defaultEditCosts fp f)) fps'
  let sorted = L.sortBy (\(_, d1) (_, d2) -> compare d1 d2) costs
  return $ map fst sorted

-- | Known Pencil errors that we know how to either recover from or quit
-- gracefully.
data PencilException
  = NotTextFile IOError
  -- ^ Failed to read a file as a text file.
  | FileNotFound (Maybe FilePath)
  -- ^ File not found. We may or may not know the file we were looking for.
  | VarNotInEnv T.Text FilePath
  -- ^ Variable is not in the environment. Variable name, and file where the
  -- variable was reference.
  | MissingRenderedContent FilePath
  -- ^ The page was missing its rendered content (this.content). Either a bug
  -- with Pencil, or a weird/bad Structure was built.
  deriving (Typeable, Show)

-- | Enum for file types that can be parsed and converted by Pencil.
data FileType = Html
              | Markdown
              | Css
              | Sass
              | Xml -- For RSS, ATOM, etc
              | Other
  deriving (Eq, Generic)

-- | 'Hashable' instance of @FileType@.
instance Hashable FileType

-- | A 'H.HashMap' of file extensions (e.g. @markdown@) to 'FileType'.
--
-- * 'Html': @html, htm@
-- * 'Markdown': @markdown, md@
-- * 'Css': @css@
-- * 'Sass': @sass, scss@
--
fileTypeMap :: H.HashMap String FileType
fileTypeMap = H.fromList
  [ ("html", Html)
  , ("htm", Html)
  , ("markdown", Markdown)
  , ("md", Markdown)
  , ("css", Css)
  , ("sass", Sass)
  , ("scss", Sass)
  , ("xml", Xml)
  ]

-- | Mapping of 'FileType' to the final converted format. Only contains
-- 'FileType's that Pencil will convert.
--
-- * 'Markdown': @html@
-- * 'Sass': @css@
--
extensionMap :: H.HashMap FileType String
extensionMap = H.fromList
  [ (Markdown, "html")
  , (Sass, "css")]

-- | Converts a 'FileType' into its converted webpage extension, if Pencil would
-- convert it (e.g. Markdown to HTML).
--
-- >>> toExtension Markdown
-- Just "html"
--
toExtension :: FileType -> Maybe String
toExtension ft = H.lookup ft extensionMap

-- | Takes a file path and returns the 'FileType', defaulting to 'Other' if it's
-- not a supported extension.
fileType :: FilePath -> FileType
fileType fp =
  -- takeExtension returns ".markdown", so drop the "."
  M.fromMaybe Other (H.lookup (map toLower (drop 1 (FP.takeExtension fp))) fileTypeMap)

-- | The Page is an important data type in Pencil. It contains the parsed
-- template of a file (e.g. of Markdown or HTML files). It may have template
-- directives (e.g. @${body}@) that has not yet been rendered, and an
-- environment loaded from the preamble section of the file. A Page also
-- contains 'pageFilePath', which is the output file path.
data Page = Page
  { pageEnv       :: Env
  , pageFilePath  :: FilePath
  -- ^ The rendered output path of this page. Defaults to the input file path.
  -- This file path is used to generate the self URL that is injected into the
  -- environment.
  , pageUseFilePath :: Bool
  -- ^ Whether or not this Page's URL should be used as the final URL in the
  -- render.
  , pageEscapeXml :: Bool
  -- ^ Whether or not XML/HTML tags should be escaped when rendered.
  } deriving (Eq, Show)

-- | Returns the 'Env' from a 'Page'.
getPageEnv :: Page -> Env
getPageEnv = pageEnv

-- | Sets the 'Env' in a 'Page'.
setPageEnv :: Env -> Page -> Page
setPageEnv env p = p { pageEnv = env }

-- | Sets this 'Page' as the designated final 'FilePath'.
useFilePath :: Page -> Page
useFilePath p = p { pageUseFilePath = True }

-- | Sets this 'Page' to render with escaped XML/HTML tags.
escapeXml :: Page -> Page
escapeXml p = p { pageEscapeXml = True }

-- | Applies the environment variables on the given pages.
--
-- The 'Structure' is expected to be ordered by inner-most content first (such
-- that the final, HTML structure layout is last in the list).
--
-- The returned Page contains the fully-applied environment and the nodes of the
-- fully rendered page in this.nodes. The `this.url` variable is set to the Page
-- in the Structure that specified 'useFilePath'; if no Page specified this,
-- then it defaults to the URL of the first (inner-most) Page.
--
-- Variable application. The outer environment's variables are applied down into
-- the inner environments. Once it hits the lowest environment, that page is
-- rendered (and has access to all variables defined in the parent). The page's
-- rendered content is now set as the @${body}@ variable in the environment.
-- The parent page now gets rendered with this new environment, and so on.
--
-- As an example, there is the common scenario where we have a default layout
-- (e.g. @default.html@), with the full HTML structure, but no body. It has only
-- a @${body}@ template variable inside. This is the parent layout. There is a
-- child layout, the partial called "blog-post.html", which has HTML for
-- rendering a blog post, like usage of ${postTitle} and ${postDate}. Inside
-- this, there is another child layout, the blog post content itself, which
-- defines the variables @postTitle@ and @postDate@, and may renderer parent
-- variables such as @websiteTitle@.
--
-- > +--------------+
-- > |              | <--- default.html
-- > |              |      Defines websiteTitle
-- > |  +---------+ |
-- > |  |         |<+----- blog-post.html
-- > |  | +-----+ | |      Renders ${postTitle}, ${postDate}
-- > |  | |     | | |
-- > |  | |     | | |
-- > |  | |     |<+-+----- blog-article-content.markdown
-- > |  | |     | | |      Renders ${websiteTitle}
-- > |  | +-----+ | |      Defines postTitle, postDate
-- > |  +---------+ |
-- > +--------------+
--
-- In this case, we want to accumulate the environment variables, starting from
-- default.html, to blog-post.html, and blog-article-content.markdown variables.
-- Combine all of that, then render the blog post content. This content is then
-- injected into the parent's environment as a @${body}@ variable, for use in
-- blog-post.html. Now /that/ content is injected into the parent environment's
-- @${body}@ variable, which is then used to render the full-blown HTML page.
--
apply :: Structure -> PencilApp Page
apply pages = apply_ (NE.reverse pages)

-- | Apply @Structure@ and convert to @Page@.
--
-- It's simpler to implement if NonEmpty is ordered outer-structure first (e.g.
-- HTML layout).
--
-- TODO there's a bug somewhere here. The RSS and tags page isn't rendering
apply_ :: Structure -> PencilApp Page
apply_ (Node _ page :| []) = do
  env <- asks getEnv

  -- Evaluate this page's nodes using the combined env. The default FilePath is
  -- this one, since it's the last Page.
  applyPage env (pageFilePath page) page
apply_ (Node name page :| (h : rest)) = do
  -- Apply the inner Pages to accumulate the inner environments and pages. Do it
  -- in a local env where this Page's env is merged with the current env.
  -- TODO can we simplify here? We have two env locations; the implicit location
  -- in the ReaderT, and the explicit one in the Page. Do we need to use both?
  -- if we make the ReaderT one only reflect the global envs. Well, a user can
  -- add to that env at any time in their main method, even between renders. So
  -- we need to take it into account always, merging the "local" env with the
  -- page's env. So maybe we should "accumulate" in the local env only?
  pageInner <- local (\c -> (setEnv (merge (getPageEnv page) (getEnv c)) c)) (apply_ (h :| rest))

  -- Get the inner page's rendered content, and insert into the env using the
  -- specified `name`. This assumes that the inner Page's content was rendered,
  -- above.
  --
  -- If the content is missing, throw an error. Caused by either a bug in Pencil
  -- or a weird Structure.
  env <- case getContent (getPageEnv pageInner) of
           Just content -> return $
                -- Merge the two envs, since we want the accumulation of all
                -- inner envs to build the env that build be applied for this page.
                insertEnv name (VText content) (merge (getPageEnv pageInner) (getPageEnv page))
           Nothing -> throwError (MissingRenderedContent (pageFilePath pageInner))

  -- Apply this current Page's nodes with the accumulated environment of all the
  -- inner Pages.
  --
  -- Use inner-most Page's file path, as this will be the default destination of
  -- the accumluated, final, rendered page (unless `page` has useFilePath = True)..
  applyPage (traceShowId env) (pageFilePath pageInner) page
apply_ (Nodes name pages :| rest) = do
  env <- asks getEnv
  pages' <- mapM (\p -> apply_ (Node "body" p :| rest)) pages
  return (Page (H.insert name (VEnvList (map getPageEnv pages')) env) "UNSPECIFIED_FILE_PATH" False False)

applyPage :: Env -> FilePath -> Page -> PencilApp Page
applyPage env defaultFp (Page penv fp useFp escapeXml) = do
  let env' = merge env penv

  -- Evaluate the Page's nodes with the specified environment.
  nodes <- evalNodes env' (getNodes penv) `catchError` setFilePathInException fp

  -- Insert the nodes and rendered content into the env.
  let env'' = (insertEnv "this.nodes" (VNodes nodes) .
               insertEnv "this.content" (VText (nodesToText escapeXml nodes)))
              env'

  -- Use inner-most Page's file path, as this will be the destination of the
  -- accumluated, final, rendered page.
  return $ Page env'' (if useFp then fp else defaultFp) useFp escapeXml

nodesToText :: Bool -> [PNode] -> T.Text
nodesToText escapeXml nodes =
  (if escapeXml then escapeForXml else id) (renderNodes nodes)

-- | Escape XML tags in the given Text.
escapeForXml :: T.Text -> T.Text
escapeForXml text = T.pack (XML.escapeStringForXML (T.unpack text))

-- | Helper to inject a file path into a VarNotInEnv exception. Rethrow the
-- exception afterwards.
setFilePathInException :: FilePath -> PencilException -> PencilApp a
setFilePathInException fp (VarNotInEnv var _) = throwError $ VarNotInEnv var fp
setFilePathInException _ e = throwError e

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

-- | Converts the IOError to a known 'PencilException'.
--
-- How to test errors:
--
-- @
-- import Control.Exception
-- import qualified Data.Text.IO as TIO
--
-- (\e -> print (ioe_description (e :: IOError)) >> return "") `handle` (TIO.readFile "foo")
-- @
--
toPencilException :: IOError -> Maybe PencilException
toPencilException e
  | isInvalidByteSequence e = Just (NotTextFile e)
  | isNoSuchFile e = Just (FileNotFound (ioe_filename e))
  | otherwise = Nothing

-- | Returns true if the IOError is an invalid byte sequence error. This
-- suggests that the file is a binary file.
isInvalidByteSequence :: IOError -> Bool
isInvalidByteSequence e = ioe_description e == "invalid byte sequence"

-- | Returns true if the IOError is due to missing file.
isNoSuchFile :: IOError -> Bool
isNoSuchFile e = ioe_type e == NoSuchThing

readMarkdownWriteHtml :: P.PandocMonad m => P.ReaderOptions -> P.WriterOptions -> T.Text -> m T.Text
readMarkdownWriteHtml readerOptions writerOptions content  = do
  pandoc <- P.readMarkdown readerOptions content
  P.writeHtml5String writerOptions pandoc

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

-- | Sort given @Page@s by the specified ordering function.
sortByVar :: T.Text
          -- ^ Environment variable name.
          -> (Value -> Value -> Ordering)
          -- ^ Ordering function to compare Value against. If the variable is
          -- not in the Env, the Page will be placed at the bottom of the order.
          -> [Page]
          -> [Page]
sortByVar var ordering =
  L.sortBy
    (\a b ->
      maybeOrdering ordering (H.lookup var (getPageEnv a)) (H.lookup var (getPageEnv b)))

-- | Filter by a variable's value in the environment.
filterByVar :: Bool
            -- ^ If true, include pages without the specified variable.
            -> T.Text
            -- ^ Environment variable name.
            -> (Value -> Bool)
            -> [Page]
            -> [Page]
filterByVar includeMissing var f =
  L.filter
   (\p -> M.fromMaybe includeMissing (H.lookup var (getPageEnv p) >>= (Just . f)))

-- | Given a variable (whose value is assumed to be an array of VText) and list
-- of pages, group the pages by the VText found in the variable.
--
-- For example, say each Page has a variable "tags" that is a list of tags. The
-- first Page has a "tags" variable that is an VArray [VText "a"], and the
-- second Page has a "tags" variable that is an VArray [VText "a", VText "b"].
-- The final output would be a map fromList [("a", [page1, page2]), ("b",
-- [page2])].
groupByElements :: T.Text
                -- ^ Environment variable name.
                -> [Page]
                -> H.HashMap T.Text [Page]
groupByElements var pages =
  -- This outer fold takes the list of pages, and accumulates the giant HashMap.
  L.foldl'
    (\acc page ->
      let x = H.lookup var (getPageEnv page)
      in case x of
           Just (VArray values) ->
             -- This fold takes each of the found values (each is a key in the
             -- hash map), and adds the current page (from the outer fold) into
             -- each of the key.
             L.foldl'
               (\hashmap envData ->
                 case envData of
                   -- Only insert Pages into the map if the variable is an VArray of
                   -- VText. Alter the map to either (1) insert this current
                   -- page into the existing list, or (2) create a new list (the
                   -- key has never been seen) with just this page.
                   VText val -> H.alter (\mv -> Just (page : M.fromMaybe [] mv)) val hashmap
                   _ -> hashmap)
               acc values
           _ -> acc
    )
    H.empty
    -- Reverse to keep ordering consistent inside hash map, since the fold
    -- prepends into accumulated list.
    (reverse pages)

-- | Loads file in given directory as 'Resource's.
loadResources :: (FilePath -> FilePath)
              -> Bool
              -- ^ Recursive if @True@.
              -> Bool
              -- ^ Handle as pass-throughs (file copy) if @True@.
              -> FilePath
              -> PencilApp [Resource]
loadResources fpf recursive pass dir = do
  fps <- listDir recursive dir
  if pass
    then return $ map (\fp -> Passthrough fp fp) fps
    else mapM (loadResource fpf) fps

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

----------------------------------------------------------------------
-- Environment modifications
----------------------------------------------------------------------

-- | Merges two @Env@s together, biased towards the left-hand @Env@ on duplicates.
merge :: Env -> Env -> Env
merge = H.union

-- | Insert text into the given @Env@.
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

-- | Insert @Page@s into the given @Env@.
--
-- @
-- posts <- 'Pencil.Blog.loadBlogPosts' "blog/"
-- env <- asks 'getEnv'
-- insertPages "posts" posts env
-- @
insertPages :: T.Text
            -- ^ Environment variable name.
            -> [Page]
            -- ^ @Page@s to insert.
            -> Env
            -- ^ Environment to modify.
            -> PencilApp Env
insertPages var pages env = do
  envs <- mapM
               (\p -> do
                 p' <- apply (structure p)
                 let text = renderNodes (getNodes (getPageEnv p'))
                 let penv = insertEnv "this.content" (VText text) (getPageEnv p')
                 return penv)
               pages
  return $ H.insert var (VEnvList envs) env

-- | Modify a variable in the given environment.
updateEnvVal :: (Value -> Value)
          -> T.Text
          -- ^ Environment variable name.
          -> Env
          -> Env
updateEnvVal = H.adjust

-- | Insert @Value@ into the given @Env@.
insertEnv :: T.Text
          -- ^ Environment variable name.
          -> Value
          -- ^ @Value@ to insert.
          -> Env
          -- ^ Environment to modify.
          -> Env
insertEnv = H.insert

-- | Convert known Aeson types into known Env types.
maybeInsertIntoEnv :: Env -> T.Text -> A.Value -> Env
maybeInsertIntoEnv env k v =
  case toValue v of
    Nothing -> env
    Just d -> H.insert k d env

-- | Converts an Aeson Object to an Env.
aesonToEnv :: A.Object -> Env
aesonToEnv = H.foldlWithKey' maybeInsertIntoEnv H.empty


-- | Use @Resource@ to load and render files that don't need any manipulation
-- other than conversion (e.g. Sass to CSS), or for static files that you want
-- to copy as-is (e.g. binary files like images, or text files that require no
-- other processing).
--
-- Use 'passthrough', 'loadResource' and 'loadResources' to build a @Resource@
-- from a file.
--
-- In the example below, @robots.txt@ and everything in the @images/@ directory
-- will be rendered as-is.
--
-- @
-- passthrough "robots.txt" >> render
-- loadResources id True True "images/" >> render
-- @
--
data Resource
  = Single Page
  | Passthrough FilePath FilePath
  -- ^ in and out file paths

-- | Copy file from source to output dir.
copyFile :: FilePath -> FilePath -> PencilApp ()
copyFile fpIn fpOut = do
  sitePrefix <- asks getSourceDir
  outPrefix <- asks getOutputDir
  liftIO $ D.createDirectoryIfMissing True (FP.takeDirectory (outPrefix ++ fpOut))
  liftIO $ D.copyFile (sitePrefix ++ fpIn) (outPrefix ++ fpOut)

-- | Replaces the file path's extension with @.html@.
--
-- @
-- 'load' toHtml "about.markdown"
-- @
--
toHtml :: FilePath -> FilePath
toHtml fp = FP.dropExtension fp ++ ".html"

-- | Converts a file path into a directory name, dropping the extension.
-- Pages with a directory as its FilePath is rendered as an index file in that
-- directory. For example, the @pages/about.html@ is transformed into
-- @pages\/about\/@, which 'render' would render the 'Page' to the file path
-- @pages\/about\/index.html@.
--
toDir :: FilePath -> FilePath
toDir fp = FP.replaceFileName fp (FP.takeBaseName fp) ++ "/"

-- | Replaces the file path's extension with @.css@.
--
-- @
-- 'load' toCss "style.sass"
-- @
--
toCss :: FilePath -> FilePath
toCss fp = FP.dropExtension fp ++ ".css"

-- | Converts file path into the expected extensions. This means @.markdown@
-- become @.html@, @.sass@ becomes @.css@, and so forth. See 'extensionMap' for
-- conversion table.
--
-- @
-- -- Load everything inside the "assets/" folder, renaming converted files as
-- -- expected, and leaving everything else alone.
-- 'loadResources' toExpected True True "assets/"
-- @
toExpected :: FilePath -> FilePath
toExpected fp = maybe fp ((FP.dropExtension fp ++ ".") ++) (toExtension (fileType fp))

-- | Loads a file as a 'Resource'. Use this for binary files (e.g. images) and
-- for files without template directives. Regular files are still converted to
-- their web page formats (e.g. Markdown to HTML, SASS to CSS).
--
-- @
-- -- Loads and renders the image as-is. Underneath the hood
-- -- this is just a file copy.
-- loadResource id "images/profile.jpg" >> render
--
-- -- Loads and renders to about.index
-- loadResource toHtml "about.markdown" >> render
-- @
loadResource :: (FilePath -> FilePath) -> FilePath -> PencilApp Resource
loadResource fpf fp =
  -- If we can load the Page as text file, convert to a Single. Otherwise if it
  -- wasn't a text file, then return a Passthroguh resource. This is where we
  -- finally handle the "checked" exception; that is, converting the Left error
  -- case (NotTextFile) into a Right case (Passthrough).
  fmap Single (load fpf fp)
    `catchError` handle
  -- 'handle' requires FlexibleContexts
  where handle e = case e of
                     NotTextFile _ -> return (Passthrough fp (fpf fp))
                     _ -> throwError e

-- | Loads file as a pass-through. There is no content conversion, and template
-- directives are ignored. In essence this is a file copy.
--
-- @
-- passthrough "robots.txt" >> render
-- @
--
passthrough :: FilePath -> PencilApp Resource
passthrough fp = return $ Passthrough fp fp

-- | Loads a file into a Page, rendering the file (as determined by the file
-- extension) into the proper output format (e.g. Markdown rendered to
-- HTML, SCSS to CSS). Parses the template directives and preamble variables
-- into its environment. The 'Page''s 'pageFilePath' is determined by the given
-- function, which expects the original file path, and returns the designated file
-- path.
--
-- The Page's designated file path is calculated and stored in the Page's
-- environment in the variable @this.url@. This allows the template to use
-- @${this.url}@ to refer to the designated file path.
--
-- Example:
--
-- @
-- -- Loads index.markdown with the designated file path of index.html
-- load 'toHtml' "index.markdown"
--
-- -- Keep the file path as-is
-- load id "about.html"
-- @
--
-- TODO should fpf be a "setting" much like escapeXml and useFilePath? We could
-- change the API to look like:
--
-- (toHtml . useFilePath . escapeXml) load "blah.markdown"
load :: (FilePath -> FilePath) -> FilePath -> PencilApp Page
load fpf fp = do
  (_, nodes) <- parseAndConvertTextFiles fp
  let env = findEnv nodes
  let fp' = "/" ++ fpf fp
  let env' = (H.insert "this.url" (VText (T.pack fp')) .
             -- Filter out preamble nodes, since we've already injected preamble
             -- into the env.
             H.insert "this.nodes" (VNodes (filter (not . isPreamble) nodes)))
             env
  return $ Page env' fp' False False

-- | Find preamble node, and load as an Env. If no preamble is found, return a
-- blank Env.
findEnv :: [PNode] -> Env
findEnv nodes =
  aesonToEnv $ M.fromMaybe H.empty (findPreambleText nodes >>= (A.decodeThrow . encodeUtf8 . T.strip))

-- | Loads and renders file as CSS.
--
-- @
-- -- Load, convert and render as style.css.
-- renderCss "style.sass"
-- @
renderCss :: FilePath -> PencilApp ()
renderCss fp =
  -- Drop .scss/sass extension and replace with .css.
  load toCss fp >>= render

-- | A @Structure@ is a list of 'Page's, defining a nesting order. Think of them
-- like <https://en.wikipedia.org/wiki/Matryoshka_doll Russian nesting dolls>.
-- The first element defines the outer-most container, and subsequent elements
-- are /inside/ the previous element.
--
-- You commonly use @Structure@s to insert a @Page@ containing content (e.g. a blog
-- post) into a container (e.g. a layout shared across all your web pages).
--
-- Build structures using 'structure', '<||' and '<|'.
--
-- @
-- layout <- load toHtml "layout.html"
-- index <- load toHtml "index.markdown"
-- about <- load toHtml "about.markdown"
-- render (layout <|| index)
-- render (layout <|| about)
-- @
--
-- In the example above we load a layout @Page@, which can be an HTML page
-- defining the outer structures like @\<html\>\<\/html\>@. Assuming @layout.html@
-- has the template directive @${body}@ (note that @body@ is a special variable
-- generated during structure-building), @layout <|| index@
-- tells 'render' that you want the rendered body of @index@ to be injected into
-- the @${body}@ directive inside of @layout@.
--
-- @Structure@s also control the closure of variables. Variables defined in a
-- @Page@s are accessible both by @Page@s above and below. This allows inner
-- @Page@s to define variables like the blog post title, which may be used in
-- the outer @Page@ to set the @\<title\>@ tag.
--
-- In this way, @Structure@ allows efficient @Page@ reuse. See the private
-- function 'apply' to learn more about how @Structure@s are
-- evaluated.
--
-- Note that this differs from the @${partial(...)}@ directive, which has no
-- such variable closures. The partial directive is much simpler—think of them
-- as copy-and-pasting snippets from one file to another. A partial has
-- the same environment as the parent context.
type Structure = NonEmpty Node

data Node =
    Node T.Text Page
  | Nodes T.Text [Page]

-- | Creates a new @Structure@ from two @Page@s.
--
-- @
-- layout <- load toHtml "layout.html"
-- index <- load toHtml "index.markdown"
-- render (layout <|| index)
-- @
(<||) :: Page -> Page -> Structure
(<||) x y = Node "body" y :| [Node "body" x]

-- | Pushes @Page@ into @Structure@.
--
-- @
-- layout <- load toHtml "layout.html"
-- blogLayout <- load toHtml "blog-layout.html"
-- blogPost <- load toHtml "myblogpost.markdown"
-- render (layout <|| blogLayout <| blogPost)
-- @
(<|) :: Structure -> Page -> Structure
(<|) s p = NE.cons (Node "body" p) s

-- | Pushes @Node@ into the @Structure@.
(<<|) :: Structure -> Node -> Structure
(<<|) s node = NE.cons node s

-- | Creates a collection 'Node'.
coll :: T.Text -> [Page] -> Node
coll = Nodes

-- | Converts a @Page@ into a @Structure@.
structure :: Page -> Structure
structure p = Node "body" p :| []

-- | Runs the computation with the given environment. This is useful when you
-- want to render a 'Page' or 'Structure' with a modified environment.
--
-- @
-- withEnv ('insertText' "newvar" "newval" env) ('render' page)
-- @
--
withEnv :: Env -> PencilApp a -> PencilApp a
withEnv env = local (setEnv env)

-- | To render something is to create the output web pages, rendering template
-- directives into their final form using the current environment.
class Render a where
  -- | Renders 'a' as web page(s).
  render :: a -> PencilApp ()

instance Render Resource where
  render (Single page) = render page
  render (Passthrough fpIn fpOut) = copyFile fpIn fpOut

-- This requires FlexibleInstances.
instance Render Structure where
  render s = apply s >>= render

instance Render Page where
  render page = do
    let fpOut = pageFilePath page
    outPrefix <- asks getOutputDir
    let noFileName = FP.takeBaseName fpOut == ""
    let fpOut' = outPrefix ++ if noFileName then fpOut ++ "index.html" else fpOut
    liftIO $ D.createDirectoryIfMissing True (FP.takeDirectory fpOut')
    liftIO $ TIO.writeFile fpOut' (renderNodes (getNodes (getPageEnv page)))

-- This requires FlexibleInstances.
instance Render r => Render [r] where
  render rs = forM_ rs render
