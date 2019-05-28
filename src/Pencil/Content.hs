{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Load, compose and render content.
-}
module Pencil.Content
  (
  -- ** Page

    Page
  , load
  , load'
  , loadDir
  , loadDir'
  , loadAndRender
  , rename
  , to
  , move
  , useFilePath
  , escapeXml
  , getPageEnv, setPageEnv
  , filterByVar
  , sortByVar
  , groupByElements
  , insertPages

  -- ** Structure

  , Structure
  , struct
  , (<||)
  , (<|)
  , (<<|)
  , coll

  -- ** Resource

  , Resource
  , passthrough
  , loadResource
  , loadResources

  -- ** Render

  , Render(..)

  -- ** File paths and types

  , listDir
  , toExpected
  , toHtml
  , toCss
  , toDir

  , FileType
  , fileType
  , toExtension

  , HasFilePath(..)
  ) where

import Pencil.App.Internal
import Pencil.Config
import Pencil.Content.Internal
import Pencil.Content.Nodes
import Pencil.Env
import Pencil.Env.Internal
import Pencil.Parser

import Control.Monad (forM_, foldM, filterM)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.Pandoc.XML as XML


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
-- Page
----------------------------------------------------------------------

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
apply_ (Node _ page :| []) = do
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
               insert "this.nodes" (VNodes nodes) .
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

-- | Sorts pages by an ordering function.
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

-- | Filters pages by a variable's value in the environment.
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

-- | Loads a file as a page, rendering the file (as determined by the file
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
-- 'render' $ 'rename' 'toDir' \<$\> load "pages/about.markdown"
-- @
--
load :: FilePath -> PencilApp Page
load fp = rename toExpected <$> load' fp

-- | Like 'load', loads a file as a page. Unlike 'load', the source file path
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

-- | A version of load' for directories. Loads the files in the specified
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

-- | Internal implemenation for loading directories to pgaes.
loadDirWith :: (FilePath -> PencilApp Page)
            -> Bool
            -- ^ Recursive if true
            -> FilePath
            -> PencilApp [Page]
loadDirWith loadF recur fp = do
  fps <- listDir recur fp
  foldM
    (\acc fpath -> do
      mp <- (loadF fpath >>= return . Just) `catchError` handle
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
-- blogPosts <- 'Pencil.Blog.loadPosts' "posts/"
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
-- posts <- 'Pencil.Blog.loadPosts' "blog/"
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
