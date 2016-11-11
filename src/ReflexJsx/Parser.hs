{-| The Parser for the reflex-jsx language

    Given a "String", @parseJsx@ outputs the AST for the language. Note that at
    this point, we capture spliced expressions from the meta-language as
    Strings, and parse them during the quasiquoting phase in @ReflexJsx.QQ@
-}
module ReflexJsx.Parser
       ( parseJsx
       , Node(..)
       , Attrs
       ) where

import Text.Parsec (runParser, Parsec, try, eof, many, many1, manyTill, between)
import Text.Parsec.Char (char, anyChar, letter, oneOf, noneOf, string, alphaNum, spaces)

import Control.Applicative ((<|>))

type Attrs = [(String, String)]

data Node = Node String Attrs [Node]
          | Text String
          | SplicedNode String
          deriving Show


parseJsx :: Monad m => String -> m Node
parseJsx s =
  case runParser p () "" s of
    Left err -> fail $ show err
    Right e -> return e
  where p = do
          spaces
          node <- jsxElement
          spaces
          eof
          return node


jsxElement :: Parsec String u Node
jsxElement = do
  try jsxSelfClosingElement <|> jsxNormalElement


jsxSelfClosingElement :: Parsec String u Node
jsxSelfClosingElement = do
  _ <- char '<'
  name  <- jsxIdentifier
  attrs <- jsxNodeAttrs
  _ <- string "/>"
  return (Node name attrs [])


jsxNormalElement :: Parsec String u Node
jsxNormalElement = do
  (name, attrs) <- jsxOpeningElement
  children <- many jsxChild
  jsxClosingElement name
  return (Node name attrs children)


jsxOpeningElement :: Parsec String u (String, Attrs)
jsxOpeningElement = do
  _ <- char '<'
  name <- jsxIdentifier
  attrs <- jsxNodeAttrs
  _ <- char '>'
  return (name, attrs)

jsxNodeAttrs :: Parsec String u Attrs
jsxNodeAttrs = many jsxNodeAttr

jsxNodeAttr :: Parsec String u (String, String)
jsxNodeAttr = do
  key <- jsxAttributeName
  spaces
  _ <- char '='
  spaces
  value <- jsxQuotedValue
  spaces
  return (key, value)

jsxAttributeName :: Parsec String u String
jsxAttributeName = many $ letter <|> char '-'

jsxQuotedValue :: Parsec String u String
jsxQuotedValue = do
  contents <- between (char '"') (char '"') $ many (noneOf "\"")
  return contents

jsxClosingElement :: String -> Parsec String u ()
jsxClosingElement ele = do
  _ <- string "</" *> string ele *> char '>'
  return ()

jsxChild :: Parsec String u Node
jsxChild = do
  try jsxText <|> try jsxSplicedNode <|> try jsxElement

jsxText :: Parsec String u Node
jsxText = do
  contents <- many1 $ noneOf "{<>}"
  return $ Text contents

jsxSplicedNode :: Parsec String u Node
jsxSplicedNode = do
  exprString <- between (char '{') (char '}') $ many (noneOf "}")
  return $ SplicedNode exprString

jsxIdentifier :: Parsec String u String
jsxIdentifier = do
  name <- many1 alphaNum
  spaces
  return name
