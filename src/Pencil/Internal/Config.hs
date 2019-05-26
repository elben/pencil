{-|
Pencil Config.
-}
module Pencil.Internal.Config where


import Data.Default (Default)
import Pencil.Internal.Env
import Text.Pandoc.Extensions (disableExtension, Extension(..))
import Text.Sass.Options (defaultSassOptions)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Highlighting
import qualified Text.Sass as Sass
import qualified Text.Sass as Sass

-- | The main @Config@ needed to build your website. Your app's @Config@ is
-- passed into the 'PencilApp' monad transformer.
--
-- Use 'defaultConfig' as a starting point, along with the config-modification
-- helpers such as 'setSourceDir'.
--
data Config = Config
  { configSourceDir :: FilePath
  , configOutputDir :: FilePath
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
--  , 'configEnv' = HashMap.empty
--  , 'configDisplayValue' = 'toText'
--  , 'configSassOptions' = Text.Sass.Options.defaultSassOptions
--  , 'configPandocReaderOptions' = Text.Pandoc.def {
--       Text.Pandoc.readerExtensions = 'Text.Pandoc.Extensions.disableExtension' 'Text.Pandoc.Extensions.Ext_tex_math_dollars' ('Text.Pandoc.Extensions.getDefaultExtensions' "markdown")
--    }
--  , 'configPandocWriterOptions' = Text.Pandoc.def { Text.Pandoc.writerHighlightStyle = Just Text.Pandoc.Highlighting.monochrome }
--  , 'configDisplayValue = 'toText'
--  }
-- @
--
-- @Ext_tex_math_dollars@ is disabled because it messes with parsing template
-- variable directives. If you want TeX math, the better option is drop in a
-- JavaScript library like KaTeX (https://katex.org).
--
defaultConfig :: Config
defaultConfig = Config
  { configSourceDir = "site/"
  , configOutputDir = "out/"
  , configEnv = H.empty
  , configSassOptions = Text.Sass.Options.defaultSassOptions

  -- For markdown reader. We use getDefaultExtensions "markdown" here to get default Markdown extensions.
  -- See
  -- https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Extensions.html#v:getDefaultExtensions
  --
  -- Ext_text_math_dollars is disabled because it messes with variable
  -- directives. For example, this renders weird (as of Pandoc 2.7.2):
  -- > **${name}** and **${age}**
  , configPandocReaderOptions = P.def {
      P.readerExtensions = disableExtension Ext_tex_math_dollars (P.getDefaultExtensions "markdown")
  }
  , configPandocWriterOptions = P.def {
      P.writerHighlightStyle = Just Text.Pandoc.Highlighting.monochrome
  }
  , configDisplayValue = toText
  }

-- | Gets the source directory of your web page source files.
getSourceDir :: Config -> FilePath
getSourceDir = configSourceDir

-- | Sets the source directory of your web page source files.
setSourceDir :: FilePath -> Config -> Config
setSourceDir fp c = c { configSourceDir = fp }

-- | Gets the output directory of your rendered web pages.
getOutputDir :: Config -> FilePath
getOutputDir = configOutputDir

-- | Sets the output directory of your rendered web pages.
setOutputDir :: FilePath -> Config -> Config
setOutputDir fp c = c { configOutputDir = fp }

-- | Gets environment of the @Config@, which is what the @PencilApp@ monad
-- transformer uses. This is where variables are set for rendering template
-- directives.
getEnv :: Config -> Env
getEnv = configEnv

-- | Sets the current environment. You may also want to look at 'withEnv' if you
-- want to 'render' things in a modified environment.
setEnv :: Env -> Config -> Config
setEnv env c = c { configEnv = env }

-- | Updates the Env inside the 'Config'.
updateEnv :: (Env -> Env) -> Config -> Config
updateEnv f c = c { configEnv = f (getEnv c) }

-- | Gets the 'Sass.SassOptions' for rendering Sass/Scss files.
getSassOptions :: Config -> Sass.SassOptions
getSassOptions = configSassOptions

-- | Sets the 'Sass.SassOptions' for rendering Sass/Scss files.
setSassOptions :: Sass.SassOptions -> Config -> Config
setSassOptions env c = c { configSassOptions = env }

-- | Gets the 'Text.Pandoc.ReaderOptions' for reading files that use Pandoc.
-- Supported formats:
--
-- * Markdown
-- * Open a GitHub issue if you'd like to see more options!
--
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

-- | Gets the 'Text.Pandoc.WriterOptions' for rendering files that use Pandoc.
getPandocWriterOptions :: Config -> P.WriterOptions
getPandocWriterOptions = configPandocWriterOptions

-- | Sets the 'Text.Pandoc.WriterOptions'.
setPandocWriterOptions :: P.WriterOptions -> Config -> Config
setPandocWriterOptions o c = c { configPandocWriterOptions = o }

-- | Gets the function that renders 'Value' to text.
getDisplayValue :: Config -> Value -> T.Text
getDisplayValue = configDisplayValue

-- | Sets the function that renders 'Value' to text. Overwrite this with your
-- own function if you would like to change how certain 'Value's are rendered
-- (e.g. 'Pencil.Internal.Env.VDateTime').
--
-- @
-- myRender :: Value -> T.Text
-- myRender (VDateTime dt) = 'T.pack' $ 'TF.formatTime' 'TF.defaultTimeLocale' "%e %B %Y" dt
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
