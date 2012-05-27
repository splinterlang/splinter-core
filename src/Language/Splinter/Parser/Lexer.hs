module Language.Splinter.Parser.Lexer where

import Control.Monad
import Data.Char

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T

import Language.Splinter.Parser.ParserType

splinterLangDef :: Monad m => T.GenLanguageDef String u m
splinterLangDef = T.LanguageDef
  { T.commentStart    = "/-"
  , T.commentEnd      = "-/"
  , T.commentLine     = "--"
  , T.nestedComments  = True
  , T.identStart      = letter <|> char '_'
  , T.identLetter     = alphaNum <|> char '_'
  , T.opStart         = opChars
  , T.opLetter        = opChars
  , T.reservedOpNames = []
  , T.reservedNames   = reservedNames
  , T.caseSensitive   = True
  }

opChars :: Monad m => ParsecT String u m Char
opChars = oneOf ":!#$%&*+./<=>?@\\^|-~"

reservedNames = ["module", "imports", "exports", "as"]

-- Special identifier parsers to handle capitalizatoin rules.

upperIdent :: EParser String
upperIdent = T.identifier $ T.makeTokenParser 
           $ splinterLangDef { T.identStart = oneOf ['A'..'Z'] }

lowerIdent :: EParser String
lowerIdent  = T.identifier $ T.makeTokenParser
            $ splinterLangDef { T.identStart = oneOf ['a'..'z'] }

-- Box up the names from the lexer nicely.

lexer :: Monad m => T.GenTokenParser String u m
lexer = T.makeTokenParser splinterLangDef

parens :: EParser a -> EParser a
parens = T.parens lexer

operator :: EParser String
operator = T.operator lexer

identifier :: EParser String
identifier = T.identifier lexer

lexeme :: EParser a -> EParser a
lexeme = T.lexeme lexer

comma :: EParser String
comma = T.comma lexer

whiteSpace :: EParser ()
whiteSpace = T.whiteSpace lexer

commaSep1 :: EParser a -> EParser [a]
commaSep1 = T.commaSep1 lexer

commaSep :: EParser a -> EParser [a]
commaSep = T.commaSep lexer

symbol :: String -> EParser String
symbol = T.symbol lexer

reserved :: String -> EParser ()
reserved = T.reserved lexer

reservedOp :: String -> EParser ()
reservedOp = T.reservedOp lexer

stringLiteral :: EParser String
stringLiteral = T.stringLiteral lexer

charLiteral :: EParser Char
charLiteral = T.charLiteral lexer

rational :: EParser Rational
rational = liftM (either toRational toRational) (T.naturalOrFloat lexer)

number :: Integer -> EParser Char -> EParser Integer 
number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }

float :: EParser Double
float = T.float lexer

decimal :: EParser Integer
decimal = T.decimal lexer

octal :: EParser Integer
octal = T.octal lexer

hexadecimal :: EParser Integer
hexadecimal = T.hexadecimal lexer

parseLex :: EParser a -> EParser a
parseLex p = do 
  whiteSpace
  x <- p
  eof
  return x
