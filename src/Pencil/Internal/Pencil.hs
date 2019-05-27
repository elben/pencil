{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Internal implementation of Pencil's functionality.
-}
module Pencil.Internal.Pencil where

import Pencil.Env
import Pencil.App
import Pencil.Parser.Internal
import Pencil.Config
import Pencil.Content

import Control.Exception (tryJust)
import Control.Monad (forM_, foldM, filterM)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Char (toLower)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException(ioe_description, ioe_filename, ioe_type), IOErrorType(NoSuchThing))
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Format as TF
import qualified Data.Yaml as A
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.Pandoc as P
import qualified Text.Pandoc.XML as XML
import qualified Text.Sass as Sass

-- | Enum for file types that can be parsed and converted by Pencil.
data FileType = Html
              | Markdown
              | Css
              | Sass
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

----------------------------------------------------------------------
-- Page
----------------------------------------------------------------------

-- | Gets the Env of a 'Page'.
getPageEnv :: Page -> Env
getPageEnv = pageEnv

-- | Sets the Env of a 'Page'.
setPageEnv :: Env -> Page -> Page
setPageEnv env p = p { pageEnv = env }

-- | Sets this 'Page' as the designated final 'FilePath'.
--
-- This is useful when you are building a 'Structure' but don't want the file
-- path of the last 'Page' in the structure to be the destination file path on
-- render.
--
-- The [Pages and
-- Structures](https://elbenshira.com/pencil/guides/pages-and-structures/) guide
-- describes this in detail.
--
-- @
-- a <- load "a.html"
-- b <- load "b.html"
-- c <- load "c.html"
--
-- -- Rendered file path is "c.html"
-- render $ a <|| b <| c
--
-- -- Rendered file path is "b.html"
-- render $ a <|| useFilePath b <| c
-- @
--
useFilePath :: Page -> Page
useFilePath p = p { pageUseFilePath = True }

-- | Sets this 'Page' to render with escaped XML/HTML tags.
--
-- This is useful when you are building an RSS feed, and you need the /contents/
-- of each item in the feed to HTML-escaped.
--
-- @
-- rss <- load "rss.xml"
-- item1 <- load "item1.html"
--
-- render $ rss <|| escapeXml item1
-- @
--
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
apply structure = do
  let reversed = NE.reverse (structureNodes structure)

  -- Collections cannot be the first element in the structure. It would be
  -- useless there, since nothing can reference it.
  let h = NE.head reversed
  when (isColl h) $ throwError (CollectionFirstInStructure (nodeName h))

  setFilePath (getFilePath structure) <$> apply_ reversed

-- | Apply list of @Page@s and convert to @Page@.
--
-- It's simpler to implement if NonEmpty is ordered outer-structure first (e.g.
-- HTML layout).
--
apply_ :: NE.NonEmpty Node -> PencilApp Page
apply_ (Node name page :| []) = do
  env <- asks getEnv

  -- Evaluate this page's nodes using the combined env
  applyPage env page

apply_ (Nodes name pages :| rest) = do
  -- Collection nodes must be the last element in the Structure. Before, you
  -- could attach more Pages after a collection (such that each page in the
  -- collection received the rest of the structure's env.)
  --
  -- But we now disallow this for a couple of reasons:
  --
  -- - Most use-cases of collections involve it being the last item anyways
  --   (e.g. list of blog posts)
  -- - Simplifes logic of choosing with Page becomes the default file path to be
  --   used in the Structure render. The rule is now: the last non-collection
  --   Page becomes the default file name. It was complicated with collections,
  --   since a collection had N pages to choose from. And most use-cases (e.g.
  --   list of blog posts) would then require you to call `useFilePath` for the
  --   last Page to specify.
  when (not (null rest)) $ throwError (CollectionNotLastInStructure name)

  env <- asks getEnv

  -- Apply the inner pages against the rest of the Structure.
  pages' <- mapM (\p -> apply_ (Node "body" p :| rest)) pages

  return $
    Page ((H.insert name (VEnvList (map getPageEnv pages')) env)) "UNSPECIFIED_FILE_PATH" False False

apply_ (Node name page :| (h : rest)) = do
  -- Apply the inner Pages to accumulate the inner environments and pages. Do it
  -- in a local env where this Page's env is merged with the current env.
  pageInner <- local (\c -> setEnv (merge (getPageEnv page) (getEnv c)) c) (apply_ (h :| rest))

  -- At this point, pageInner's env is the accumulate of this page's env and all
  -- the inner pages' envs.
  --
  -- Get the inner page's rendered content, and insert into a new env using the
  -- specified `name`. This assumes that the inner Page's content was rendered,
  -- above. Some pages don't have this.content (e.g. 'Nodes' pages).
  let env' = maybe (getPageEnv pageInner)
              (\content -> insertText name content (getPageEnv pageInner))
              (getContent (getPageEnv pageInner))

  -- Apply this current Page's nodes with the accumulated environment of all the
  -- inner Pages.
  applyPage env' page

-- | Applies the Page by merging the given env with the Page's env, evaluating
-- the nodes with the combined env, and generating a new env for the Page,
-- containing @this.url@, @this.nodes@ and @this.content@ in the env.
applyPage :: Env -> Page -> PencilApp Page
applyPage env page = do
  let env' = merge (getPageEnv page) env

  -- Evaluate the Page's nodes with the specified environment.
  nodes <- evalNodes env' (getNodes (getPageEnv page))

  -- Generate this Page's final file path.
  -- Insert the nodes and rendered content into the env.
  let env'' = (insertText "this.url" (T.pack ("/" ++ pageFilePath page)) .
               insertEnv "this.nodes" (VNodes nodes) .
               insertText "this.content" (nodesToText (pageEscapeXml page) nodes))
              env'

  -- Use inner-most Page's file path, as this will be the destination of the
  -- accumluated, final, rendered page.
  return $ page { pageEnv = env'' }

-- | Render nodes. XML/HTML tags are escaped if @escpXml@ is True.
nodesToText :: Bool
            -- ^ XML/HTML tags escaped if True.
            -> [PNode]
            -> T.Text
nodesToText escXml nodes =
  (if escXml then escapeForXml else id) (renderNodes nodes)

-- | Escape XML tags in the given Text.
escapeForXml :: T.Text -> T.Text
escapeForXml text = T.pack (XML.escapeStringForXML (T.unpack text))

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

-- | Converts Markdown to HTML using the given options.
readMarkdownWriteHtml :: P.PandocMonad m => P.ReaderOptions -> P.WriterOptions -> T.Text -> m T.Text
readMarkdownWriteHtml readerOptions writerOptions content = do
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

-- | Sorts given @Page@s by the specified ordering function.
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

-- | Filters by a variable's value in the environment.
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
-- of pages, groups the pages by the VText found in the variable.
--
-- For example, say each Page has a variable "tags" that is a list of tags. The
-- first Page has a "tags" variable that is an @VArray [VText "a"]@, and the
-- second Page has a "tags" variable that is an @VArray [VText "a", VText "b"]@.
-- The final output would be a map @fromList [("a", [page1, page2]), ("b",
-- [page2])]@.
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

-- | Returns True if the file path is a directory.
-- Examples: foo/bar/
-- Examples of not directories: /foo, foo/bar, foo/bar.baz
isDir :: FilePath -> Bool
isDir fp = null (FP.takeBaseName fp)



-- | @Resource@ is used to copy static binary files to the destination, and to
-- load and render files that just needs conversion without template directives
-- or structures.
--
-- This is how Pencil handles files like images, compiled JavaScript, or text
-- files that require only a straight-forward conversion.
--
-- Use 'passthrough', 'loadResource' and 'loadResources' to build a @Resource@
-- from a file.
--
-- In the example below, @robots.txt@ and everything in the @images/@ directory
-- will be rendered as-is.
--
-- @
-- passthrough "robots.txt" >>= render
-- passthrough "images/" >>= render
-- @
--
data Resource
  = Single Page
  | Passthrough FilePath FilePath
  -- ^ in and out file paths (can be dir or files)

-- | Copy file from source to output dir. If both the input and output file
-- paths are directories, recursively copy the contents from one to the other.
copyFile :: FilePath -> FilePath -> PencilApp ()
copyFile fpIn fpOut = do
  sitePrefix <- asks getSourceDir
  outPrefix <- asks getOutputDir
  liftIO $ D.createDirectoryIfMissing True (FP.takeDirectory (outPrefix ++ fpOut))

  if isDir fpIn && isDir fpOut
    -- Copying directories
    then do
      fps <- listDir True fpIn
      forM_ fps (\fp -> copyFile fp (fpOut ++ (FP.takeFileName fp)))
    else
      liftIO $ D.copyFile (sitePrefix ++ fpIn) (outPrefix ++ fpOut)

-- | Replaces the file path's extension with @.html@.
--
-- @
-- rename toHtml \<$\> 'load' "about.htm"
-- @
--
toHtml :: FilePath -> FilePath
toHtml fp = FP.dropExtension fp ++ ".html"

-- | Converts a file path into a directory name, dropping the extension.
-- Pages with a directory as its file path is rendered as an index file in that
-- directory.
--
-- For example, @pages/about.html@ is transformed into @pages\/about\/@, which
-- upon 'render' results in the destination file path @pages\/about\/index.html@:
--
-- @
-- toDir "pages/about.html"
-- @
--
-- Load and render as @pages\/about\/@:
--
-- @
-- render $ 'rename' toDir \<$\> 'load' "pages/about.html"
-- @
--
toDir :: FilePath -> FilePath
toDir fp = FP.replaceFileName fp (FP.takeBaseName fp) ++ "/"

-- | Replaces the file path's extension with @.css@.
--
-- @
-- rename toCss \<$\> 'load' "style.sass"
-- @
--
toCss :: FilePath -> FilePath
toCss fp = FP.dropExtension fp ++ ".css"

-- | Converts file path into the expected extensions. This means @.markdown@
-- become @.html@, @.sass@ becomes @.css@, and so forth. See 'extensionMap' for
-- conversion table.
toExpected :: FilePath -> FilePath
toExpected fp = maybe fp ((FP.dropExtension fp ++ ".") ++) (toExtension (fileType fp))

-- | Loads a file as a 'Resource'. Use this for binary files (e.g. images) and
-- for files without template directives that may still need conversion (e.g.
-- Markdown to HTML, SASS to CSS).
--
-- Generally, you can just use `loadAndRender` instead of this method.
--
-- Loads and renders the image as-is. Underneath the hood this is just a file
-- copy:
--
-- > loadResource "images/profile.jpg" >>= render
--
-- Loads and renders to @about.html@:
--
-- > loadResource "about.markdown" >>= render
--
loadResource :: FilePath -> PencilApp Resource
loadResource fp =
  -- If we can load the Page as text file, convert to a Single. Otherwise if it
  -- wasn't a text file, then return a Passthroguh resource. This is where we
  -- finally handle the "checked" exception; that is, converting the Left error
  -- case (NotTextFile) into a Right case (Passthrough).
  fmap Single (load fp)
    `catchError` handle
  -- 'handle' requires FlexibleContexts
  where handle e = case e of
                     NotTextFile _ -> return (Passthrough fp fp)
                     _ -> throwError e

-- | Loads file in given directory as 'Resource's.
--
-- Generally, you can just use `loadAndRender` instead of this method.
--
-- Load everything inside the @assets/@ folder, renaming converted files as
-- expected (e.g. SCSS to CSS):
--
-- > loadResources True True "assets/"
--
loadResources :: Bool
              -- ^ Recursive if @True@.
              -> Bool
              -- ^ Handle as pass-throughs (file copy) if @True@.
              -> FilePath
              -> PencilApp [Resource]
loadResources recursive pass dir = do
  fps <- listDir recursive dir
  if pass
    then return $ map (\fp -> Passthrough fp fp) fps
    else mapM loadResource fps

-- | Loads file as a pass-through. There is no content conversion, and template
-- directives are ignored. In essence this is a file copy.
--
-- @
-- passthrough "robots.txt" >>= render
--
-- render (move "images\/profile.jpg" \<$\> passthrough "images\/myProfile.jpg")
-- @
--
passthrough :: FilePath -> PencilApp Resource
passthrough fp = return $ Passthrough fp fp

-- | Loads a file into a Page, rendering the file (as determined by the file
-- extension) into the proper output format (e.g. Markdown rendered to
-- HTML, SCSS to CSS). Parses the template directives and preamble variables
-- into its environment.
--
-- This loads @index.markdown@ with the destination file path set to @index.html@:
--
-- > load "index.markdown"
--
-- Because this is already an HTML file, the file path is kept as @about.html@:
--
-- > load "about.html"
--
-- Using 'rename' and 'toDir', the destination file path becomes @pages\/about\/index.html@:
--
-- @
-- 'rename' 'toDir' <$> load "pages/about.markdown"
-- @
--
load :: FilePath -> PencilApp Page
load fp = rename toExpected <$> load' fp

-- | Like 'load', loads a file into a Page. Unlike 'load', the source file path
-- is used as the destination file path (i.e. the extension name is not changed).
--
load' :: FilePath -> PencilApp Page
load' fp = do
  (_, nodes) <- parseAndConvertTextFiles fp
  -- Filter out preamble nodes, since we've already injected preamble into the
  -- env.
  let env' = H.insert "this.nodes" (VNodes (filter (not . isPreamble) nodes)) (findEnv nodes)
  return $ Page env' fp False False

-- | A version of 'load' for directories.
--
-- @
-- layout <- load "layout.html"
-- tutorials <- loadDir False "tutorials/"
-- render $ fmap ((layout '<||') . 'rename' 'toDir') tutorials
-- @
loadDir :: Bool
        -- ^ If True, recursively load files in the directory
        -> FilePath
        -> PencilApp [Page]
loadDir = loadDirWith load

-- | A version of 'load'' for directories. Loads the files in the specified
-- directory as pages. Keeps the original file path.
--
-- @
-- tutorials <- loadDir' False "tutorials/"
-- render $ fmap ((layout <||) . rename toDir) pages
-- @
loadDir' :: Bool
         -- ^ Recursive if true
         -> FilePath
         -> PencilApp [Page]
loadDir' = loadDirWith load'

loadDirWith :: (FilePath -> PencilApp Page)
            -> Bool
            -- ^ Recursive if true
            -> FilePath
            -> PencilApp [Page]
loadDirWith loadF recur fp = do
  fps <- listDir recur fp
  foldM
    (\acc fp -> do
      mp <- (loadF fp >>= return . Just) `catchError` handle
      return (maybe acc (\p -> p : acc) mp))
    []
    (reverse fps)
  where handle e = case e of
                    NotTextFile _ -> return Nothing
                    _ -> throwError e

-- | Loads and renders file, converting content if it's convertible (e.g.
-- Markdown to HTML). The final file path is the "default conversion", if Pencil
-- knows how to convert the file (e.g. .markdown to .html). Otherwise, the same
-- file name is kept (e.g. .txt).
--
-- Load @style.sass@, convert to CSS, and render as @style.css@:
--
-- > loadAndRender "style.sass"
--
-- Load, convert and render everything in the @assets/@ folder. Binary files are
-- copied as-is without any further processing:
--
-- > loadAndRender "assets/"
--
loadAndRender :: FilePath -> PencilApp ()
loadAndRender fp =
  if isDir fp
    then loadResources True False fp >>= render
    else loadResource fp >>= render

-- | Returns True if the given Node is a collection node.
isColl :: Node -> Bool
isColl (Nodes _ _) = True
isColl _ = False

-- | Get the node's name.
nodeName :: Node -> T.Text
nodeName (Node n _) = n
nodeName (Nodes n _) = n

-- | Creates a new structure from two pages. Pronounced "smash".
--
-- @
-- layout <- load "layout.html"
-- index <- load "index.markdown"
-- render (layout <|| index)
-- @
(<||) :: Page -> Page -> Structure
(<||) x y = Structure
  { structureNodes = Node "body" y :| [Node "body" x]
  , structureFilePath = getFilePath y
  , structureFilePathFrozen = False
  }

-- | Pushes @Page@ into @Structure@. Pronounced "push".
--
-- @
-- layout <- load "layout.html"
-- blogLayout <- load "blog-layout.html"
-- blogPost <- load "myblogpost.markdown"
-- render (layout <|| blogLayout <| blogPost)
-- @
(<|) :: Structure -> Page -> Structure
(<|) s p = s { structureNodes = NE.cons (Node "body" p) (structureNodes s)
             , structureFilePath = if structureFilePathFrozen s then structureFilePath s else getFilePath p
             , structureFilePathFrozen = structureFilePathFrozen s || pageUseFilePath p
             }

-- | Pushes @Node@ into the @Structure@. Usually used in conjunction with 'coll'.
--
-- @
-- blogLayout <- load "blog-layout.html"
-- blogPosts <- 'Pencil.Blog.loadBlogPosts' "posts/"
-- render (struct blogLayout <<| coll "posts" blogPosts)
-- @
(<<|) :: Structure -> Node -> Structure
(<<|) s node =
  let (fp, frozen) = if structureFilePathFrozen s
      then
        (structureFilePath s, True)
      else
        case node of
          -- If collection, use the existing file path (ignore file paths in
          -- collection). This keeps with the rule that the default file path
          -- is the last non-collection page.
          Nodes _ _ -> (structureFilePath s, False)
          Node _ p -> (getFilePath p, pageUseFilePath p)
  in s { structureNodes = NE.cons node (structureNodes s)
       , structureFilePath = fp
       , structureFilePathFrozen = frozen
       }

-- | Creates a collection 'Node'. Usually used in conjunction with '<<|'.
coll :: T.Text -> [Page] -> Node
coll = Nodes

-- | Converts a @Page@ into a @Structure@. This is a "singleton" structure.
struct :: Page -> Structure
struct p = Structure
  { structureNodes = Node "body" p :| []
  , structureFilePath = getFilePath p
  , structureFilePathFrozen = pageUseFilePath p
  }

-- | Runs the computation with the given environment. This is useful when you
-- want to render a 'Page' or 'Structure' with a modified environment.
--
-- @
-- withEnv ('insertText' "newvar" "newval" env) ('render' page)
-- @
--
-- Alternatively, use 'Reader.local', which is re-exported in the Pencil module.
--
withEnv :: Env -> PencilApp a -> PencilApp a
withEnv env = local (setEnv env)

----------------------------------------------------------------------
-- HasFilePath class
----------------------------------------------------------------------

-- | Class for types that has a final file path for rendering.
--
-- This allows file-path-changing methods to be re-used across 'Page',
-- 'Structure' and 'Resource' types.
class HasFilePath a where
  getFilePath :: a -> FilePath
  setFilePath :: FilePath -> a -> a

instance HasFilePath Page where
  getFilePath = pageFilePath
  setFilePath fp p = p { pageFilePath = fp }

instance HasFilePath Resource where
  getFilePath (Single p) = getFilePath p
  getFilePath (Passthrough _ fp) = fp

  setFilePath fp (Single p) = Single $ setFilePath fp p
  setFilePath fp (Passthrough ofp _) = Passthrough ofp fp

instance HasFilePath Structure where
  getFilePath = structureFilePath
  setFilePath fp s = s { structureFilePath = fp }

-- | Transforms the file path.
--
-- @
-- about <- load "about.htm"
-- render $ struct (rename 'toHtml' about)
-- @
rename :: HasFilePath a => (FilePath -> FilePath) -> a -> a
rename f a = setFilePath (f (getFilePath a)) a

-- | Sets the target file path to the specified file path. If the given file path
-- is a directory, the file name set to @index.html@. If the file path is a file
-- name, then the file is renamed.
--
-- Move @stuff/about.html@ to @about/blah.html@ on render:
--
-- > about <- to "about/blah.html" <$> load "stuff/about.htm"
--
-- Convert the destination file path to @about/index.html@:
--
-- > about <- to "about/" <$> load "stuff/about.htm"
-- > render about
--
-- Equivalent to the above example:
--
-- > about <- load "stuff/about.htm"
-- > render $ to "about/" about
--
to :: HasFilePath a => FilePath -> a -> a
to = move' "index.html"

-- | Moves the target file path to the specified file path. Behaves similar to
-- the UNIX @mv@ command: if the given file path is a directory, the file name
-- is kept the same. If the file path is a file name, then the file is renamed.
--
-- Move @assets/style.css@ to @stylesheets/style.css@:
--
-- > move "stylesheets/" <$> load "assets/style.css"
--
-- Move @assets/style.css@ to @stylesheets/base.css@.
--
-- > move "stylesheets/base.css" <$> load "assets/style.css"
--
move :: HasFilePath a => FilePath -> a -> a
move fp a = move' (FP.takeFileName (getFilePath a)) fp a

-- | Internal implemenation for 'move' and 'to'.
--
-- Moves the target file path to the specified FilePath. If the given FilePath
-- is a directory, the file name is kept the same. If the FilePath is a file
-- name, then @fromFileName@ is used as the file name.
move' :: HasFilePath a => FilePath -> FilePath -> a -> a
move' fromFileName fp a =
  let fromFileName = FP.takeFileName (getFilePath a)
      toDir = FP.takeDirectory fp
      fp' = if isDir fp
              then toDir ++ "/" ++ fromFileName
              else toDir ++ "/" ++ FP.takeFileName fp
  in setFilePath fp' a

----------------------------------------------------------------------
-- Render class
----------------------------------------------------------------------

-- | To render something is to create the output web pages, evaluating template
-- directives into their final form using the current environment.
class Render a where
  -- | Renders @a@ as web page(s).
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
    let fpOut' = outPrefix ++ if isDir fpOut then fpOut ++ "index.html" else fpOut
    liftIO $ D.createDirectoryIfMissing True (FP.takeDirectory fpOut')
    liftIO $ TIO.writeFile fpOut' (renderNodes (getNodes (getPageEnv page)))

-- This requires FlexibleInstances.
instance Render r => Render [r] where
  render rs = forM_ rs render

----------------------------------------------------------------------
-- Environment modifications
----------------------------------------------------------------------

-- | Inserts pages into the environment. The pages are evaluated and applied before insertion.
--
-- @
-- posts <- 'Pencil.Blog.loadBlogPosts' "blog/"
-- env <- asks 'getEnv'
-- env' <- insertPages "posts" posts env
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
                 p' <- apply (struct p)
                 let text = renderNodes (getNodes (getPageEnv p'))
                 let penv = insertText "this.content" text (getPageEnv p')
                 return penv)
               pages
  return $ H.insert var (VEnvList envs) env

-- | A version of 'toText' that renders 'Value' acceptable for an RSS feed.
--
-- * Dates are rendered in the RFC 822 format.
-- * Everything else defaults to the 'toText' implementation.
--
-- You'll probably want to also use 'escapeXml' to render an RSS feed.
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