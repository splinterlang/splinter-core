module Language.Splinter.Parser
  ( InputName
  , InputText
  , parseSplinter
  ) where

import Text.Parsec
import Text.Parsec.Indent
import qualified Control.Monad.Trans.State as S

import Language.Splinter.Parser.ParserType
import Language.Splinter.Parser.Lexer
import Language.Splinter.Parser.Module

type InputName = String
type InputText = String

runSplinterParser :: InputName -> InputText -> EParser a -> Either ParseError a
runSplinterParser name text parser = runIndent name $ runParserT parser () name text

parseSplinter :: InputName -> InputText -> Either ParseError ModuleDef
parseSplinter name text = runSplinterParser name text aModuleDef
