module Language.Splinter.Parser.ParserType
  ( EParser
  ) where

import Text.Parsec
import qualified Control.Monad.Trans.State as S

type EParser a = ParsecT String () (S.State SourcePos) a
