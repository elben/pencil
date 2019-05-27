{-# LANGUAGE OverloadedStrings #-}

{-|
Internal implementation of Pencil's template directive parser.
-}
module Pencil.Parser.Internal where

import Text.ParserCombinators.Parsec
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Text.Parsec as P

-- Doctest setup.
--
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Either (isLeft)

-- | Pencil's @Page@ AST.
data PNode =
    PText T.Text
  | PVar T.Text
  | PFor T.Text [PNode]
  | PIf T.Text [PNode]
  | PPartial T.Text
  | PPreamble T.Text

  -- Signals an If/For expression in the stack waiting for expressions. So that we
  -- can find the next unused open if/for-statement in nested if/for-statements.
  | PMetaIf T.Text
  | PMetaFor T.Text

  -- A terminating node that represents the end of the program, to help with AST
  -- converstion
  | PMetaEnd
  deriving (Show, Eq)

-- | Pencil's tokens for content.
data Token =
    TokText T.Text
  | TokVar T.Text
  | TokFor T.Text
  | TokIf T.Text
  | TokPartial T.Text
  | TokPreamble T.Text
  | TokEnd
  deriving (Show, Eq)

-- | Convert Tokens to PNode AST.
--
-- >>> transform [TokText "hello", TokText "world"]
-- [PText "hello",PText "world"]
--
-- >>> transform [TokIf "title", TokEnd]
-- [PIf "title" []]
--
-- >>> transform [TokIf "title", TokText "hello", TokText "world", TokEnd]
-- [PIf "title" [PText "hello",PText "world"]]
--
-- > ${if(title)}
-- >   ${for(posts)}
-- >     world
-- >   ${end}
-- > ${end}
--
-- >>> transform [TokIf "title", TokFor "posts", TokText "world", TokEnd, TokEnd]
-- [PIf "title" [PFor "posts" [PText "world"]]]
--
-- > begin
-- > now
-- > ${if(title)}
-- >   hello
-- >   world
-- >   ${if(body)}
-- >     ${body}
-- >     ${someothervar}
-- >     wahh
-- >   ${end}
-- >   final
-- >   thing
-- > ${end}
-- > the
-- > lastline
--
-- >>> transform [TokText "begin", TokText "now", TokIf "title", TokText "hello", TokText "world", TokIf "body", TokVar "body", TokVar "someothervar", TokText "wahh", TokEnd, TokText "final", TokText "thing", TokEnd, TokText "the", TokText "lastline"]
-- [PText "begin",PText "now",PIf "title" [PText "hello",PText "world",PIf "body" [PVar "body",PVar "someothervar",PText "wahh"],PText "final",PText "thing"],PText "the",PText "lastline"]
--
-- > <!--PREAMBLE
-- > foo: bar
-- > do:
-- >   - re
-- >   - me
-- > -->
-- > Hello world ${foo}
--
-- >>> transform [TokPreamble "foo: bar\ndo:\n  - re\n  -me", TokText "Hello world ", TokVar "foo"]
-- [PPreamble "foo: bar\ndo:\n  - re\n  -me",PText "Hello world ",PVar "foo"]
--
transform :: [Token] -> [PNode]
transform toks =
  let stack = ast [] toks
  in reverse stack

-- | Converts Tokens, which is just the raw list of parsed tokens, into PNodes
-- which are the tree-structure expressions (i.e. if/for nesting)
--
-- This function works by using a stack to keep track of where we are for nested
-- expressions such as if and for statements. When a token that starts a nesting
-- is found (like a TokIf), a "meta" expression (PMetaIf) is pushed into the
-- stack. When we finally see an end token (TokEnd), we pop all the expressions
-- off the stack until the first meta tag (e.g PMetaIf) is reached. All the
-- expressions popped off are now known to be nested inside that if statement.
--
ast :: [PNode] -- stack
    -> [Token] -- remaining
    -> [PNode] -- (AST, remaining)
ast stack [] = stack
ast stack (TokText t : toks) = ast (PText t : stack) toks
ast stack (TokVar t : toks)  = ast (PVar t : stack) toks
ast stack (TokPartial fp : toks) = ast (PPartial fp : stack) toks
ast stack (TokPreamble t : toks) = ast (PPreamble t : stack) toks
ast stack (TokIf t : toks)   = ast (PMetaIf t : stack) toks
ast stack (TokFor t : toks)  = ast (PMetaFor t : stack) toks
ast stack (TokEnd : toks) =
  let (node, popped, remaining) = popNodes stack
      -- ^ Find the last unused if/for statement, and grab all the expressions
      -- in-between this TokEnd and the opening if/for keyword.
      n = case node of
            PMetaIf t -> PIf t popped
            PMetaFor t -> PFor t popped
            _ -> PMetaEnd
  -- Push the statement into the stack
  in ast (n : remaining) toks

-- | Pop nodes until we hit a If/For statement.
-- Return pair (constructor found, nodes popped, remaining stack)
popNodes :: [PNode] -> (PNode, [PNode], [PNode])
popNodes = popNodes_ []

-- | Helper for 'popNodes'.
popNodes_ :: [PNode] -> [PNode] -> (PNode, [PNode], [PNode])
popNodes_ popped [] = (PMetaEnd, popped, [])
popNodes_ popped (PMetaIf t : rest) = (PMetaIf t, popped, rest)
popNodes_ popped (PMetaFor t : rest) = (PMetaFor t, popped, rest)
popNodes_ popped (t : rest) = popNodes_ (t : popped) rest

-- | Render nodes as string.
renderNodes :: [PNode] -> T.Text
renderNodes = DL.foldl' (\str n -> (T.append str (renderNode n))) ""

-- | Render node as string.
renderNode :: PNode -> T.Text
renderNode (PText t) = t
renderNode (PVar t) = T.append (T.append "${" t) "}"
renderNode (PFor t nodes) =
  let for = T.append (T.append "${for(" t) ")}"
      body = renderNodes nodes
      end = "${end}"
  in T.append (T.append for body) end
renderNode (PIf t nodes) =
  let for = T.append (T.append "${if(" t) ")}"
      body = renderNodes nodes
      end = "${end}"
  in T.append (T.append for body) end
renderNode (PPartial file) = T.append (T.append "${partial(" file) ")}"
renderNode (PMetaIf v) = renderNode (PIf v [])
renderNode (PMetaFor v) = renderNode (PFor v [])
renderNode PMetaEnd = ""
renderNode (PPreamble _) = "" -- Don't render the PREAMBLE

-- | Render tokens.
renderTokens :: [Token] -> T.Text
renderTokens = DL.foldl' (\str n -> (T.append str (renderToken n))) ""

-- | Render token.
renderToken :: Token -> T.Text
renderToken (TokText t) = t
renderToken (TokVar t) = T.append (T.append "${" t) "}"
renderToken (TokPartial fp) = T.append (T.append "${partial(\"" fp) "\"}"
renderToken (TokFor t) = T.append (T.append "${for(" t) ")}"
renderToken (TokEnd) = "${end}"
renderToken (TokIf t) = T.append (T.append "${if(" t) ")}"
renderToken (TokPreamble _) = "" -- Hide preamble content

-- | Parse text.
parseText :: T.Text -> Either ParseError [PNode]
parseText text = do
  toks <- parse parseEverything (T.unpack "") (T.unpack text)
  return $ transform toks

-- | Parse everything.
--
-- >>> parse parseEverything "" "Hello ${man} and ${woman}."
-- Right [TokText "Hello ",TokVar "man",TokText " and ",TokVar "woman",TokText "."]
--
-- >>> parse parseEverything "" "Hello ${man} and ${if(woman)} text here ${end}."
-- Right [TokText "Hello ",TokVar "man",TokText " and ",TokIf "woman",TokText " text here ",TokEnd,TokText "."]
--
-- >>> parse parseEverything "" "Hi ${for(people)} ${name}, ${end} everyone!"
-- Right [TokText "Hi ",TokFor "people",TokText " ",TokVar "name",TokText ", ",TokEnd,TokText " everyone!"]
--
-- >>> parse parseEverything "" "${realvar} $.get(javascript) $$ $$$ $} $( $45.50 $$escape $${escape2} wonderful life! ${truth}"
-- Right [TokVar "realvar",TokText " $.get(javascript) $$ $$$ $} $( $45.50 $$escape ",TokText "${",TokText "escape2} wonderful life! ",TokVar "truth"]
--
-- >>> parse parseEverything "" "<!--PREAMBLE  \n  foo: bar\ndo:\n  - re\n  -me\n  -->waffle house ${lyfe}"
-- Right [TokPreamble "  \n  foo: bar\ndo:\n  - re\n  -me\n  ",TokText "waffle house ",TokVar "lyfe"]
--
-- >>> parse parseEverything "" "YO ${foo} <!--PREAMBLE  \n  ${foo}: bar\ndo:\n  - re\n  -me\n  -->waffle house ${lyfe}"
-- Right [TokText "YO ",TokVar "foo",TokText " ",TokPreamble "  \n  ${foo}: bar\ndo:\n  - re\n  -me\n  ",TokText "waffle house ",TokVar "lyfe"]
--
-- This is a degenerate case that we will just allow (for now) to go sideways:
-- >>> parse parseEverything "" "<b>this ${var never closes</b> ${realvar}"
-- Right [TokText "<b>this ",TokVar "var never closes</b> ${realvar"]
--
parseEverything :: Parser [Token]
parseEverything =
  -- Note that order matters here. We want "most general" to be last (variable
  -- names).
  many1 (try parsePreamble
     <|> try parseEscape
     <|> try parseContent
     <|> try parseEnd
     <|> try parseFor
     <|> try parseIf
     <|> try parseEnd
     <|> try parsePartial
     <|> parseVar)

-- >>> parse parseVar "" "${ffwe} yep"
-- Right (TokVar "ffwe")
--
-- >>> parse parseVar "" "${spaces technically allowed}"
-- Right (TokVar "spaces technically allowed")
--
-- >>> isLeft $ parse parseVar "" "Hello ${name}"
-- True
--
-- >>> isLeft $ parse parseVar "" "${}"
-- True
--
-- | Parse variables.
parseVar :: Parser Token
parseVar = try $ do
  _ <- char '$'
  _ <- char '{'
  varName <- many1 (noneOf "}")
  _ <- char '}'
  return $ TokVar (T.pack varName)

-- | Parse preamble.
parsePreamble :: Parser Token
parsePreamble = do
  _ <- parsePreambleStart

  -- "Note the overlapping parsers anyChar and string "-->", and therefore the
  -- use of the try combinator."
  -- (https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec.html)
  content <- manyTill anyChar (try (string "-->"))
  return $ TokPreamble (T.pack content)

-- | Parse the start of a PREAMBLE.
parsePreambleStart :: Parser String
parsePreambleStart = string "<!--PREAMBLE"


-- | Parse partial commands.
--
-- >>> parse parsePartial "" "${partial(\"my/file/name.html\")}"
-- Right (TokPartial "my/file/name.html")
--
parsePartial :: Parser Token
parsePartial = do
  _ <- string "${partial(\""
  filename <- many (noneOf "\"")
  _ <- string "\")}"
  return $ TokPartial (T.pack filename)

-- | Parse escape sequence "$${"
--
-- >>> parse parseEscape "" "$${example}"
-- Right (TokText "${")
--
parseEscape :: Parser Token
parseEscape = do
  _ <- try $ string "$${"
  return (TokText "${")

-- | Parse boring, boring text.
--
-- >>> parse parseContent "" "hello ${ffwe} you!"
-- Right (TokText "hello ")
--
-- >>> parse parseContent "" "hello $.get() $ $( $$ you!"
-- Right (TokText "hello $.get() $ $( $$ you!")
--
-- Because of our first parser to grab a character that is not a $, we can't
-- grab strings that start with a $, even if it's text. It's a bug, just deal
-- with it for now.
-- >>> isLeft $ parse parseContent "" "$$$ what"
-- True
--
-- >>> isLeft $ parse parseContent "" "${name}!!"
-- True
--
parseContent :: Parser Token
parseContent = do
  -- The manyTill big parser below will accept an empty string, which is bad. So
  -- grab a single character to start things off.
  h <- noneOf "$"

  -- Grab chars until we see something that looks like a ${...}, or eof. Use
  -- both lookAhead (does not consume successful "${" found) and try (does not
  -- consume failure to find "${"). Not having both produces bugs, so.
  --
  -- Also grab "$${", which should be captured as an escape (parseEscape).
  --
  -- https://stackoverflow.com/questions/20020350/parsec-difference-between-try-and-lookahead
  stuff <- manyTill anyChar (try (lookAhead (string "$${")) <|>
                             try (lookAhead (string "${")) <|>
                             try (lookAhead parsePreambleStart) <|>
                             (eof >> return " "))
  return $ TokText (T.pack (h : stuff))

-- | Parse for loop declaration.
--
-- >>> parse parseFor "" "${for(posts)}"
-- Right (TokFor "posts")
--
-- >>> parse parseFor "" "${for(variable name with spaces technically allowed)}"
-- Right (TokFor "variable name with spaces technically allowed")
--
-- >>> isLeft $ parse parseFor "" "${for()}"
-- True
--
-- >>> isLeft $ parse parseFor "" "${for foo}"
-- True
--
parseFor :: Parser Token
parseFor = parseFunction "for" TokFor

-- | Parse if directive.
parseIf :: Parser Token
parseIf = parseFunction "if" TokIf

-- | General parse template functions.
parseFunction :: String -> (T.Text -> Token) -> Parser Token
parseFunction keyword ctor = do
  _ <- char '$'
  _ <- char '{'
  _ <- try $ string (keyword ++ "(")
  varName <- many1 (noneOf ")")
  _ <- char ')'
  _ <- char '}'
  return $ ctor (T.pack varName)

-- | Parse end keyword.
--
-- >>> parse parseEnd "" "${end}"
-- Right TokEnd
--
-- >>> isLeft $ parse parseEnd "" "${enddd}"
-- True
--
parseEnd :: Parser Token
parseEnd = do
  _ <- try $ string "${end}"
  return TokEnd

-- | A hack to capture strings that "almost" are templates. I couldn't figure
-- out another way.
parseFakeVar :: Parser Token
parseFakeVar = do
  _ <- char '$'
  n <- noneOf "{"
  rest <- many1 (noneOf "$")
  return $ TokText (T.pack ("$" ++ [n] ++ rest))

-- | @many1Till p end@ will parse one or more @p@ until @end.
--
-- From https://hackage.haskell.org/package/pandoc-1.10.0.4/docs/Text-Pandoc-Parsing.html
many1Till :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m end -> P.ParsecT s u m [a]
many1Till p end = do
  first <- p
  rest <- manyTill p end
  return (first : rest)

-- | Find the preamble content from the given @PNode@s.
findPreambleText :: [PNode] -> Maybe T.Text
findPreambleText nodes = DL.find isPreamble nodes >>= preambleText

-- | Returns @True@ if the @PNode@ is a @PPreamble@.
isPreamble :: PNode -> Bool
isPreamble (PPreamble _) = True
isPreamble _ = False

-- | Gets the content of the @PPreamble@.
preambleText :: PNode -> Maybe T.Text
preambleText (PPreamble t) = Just t
preambleText _ = Nothing

