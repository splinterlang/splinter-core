module HereDoc(heredoc) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
heredoc = QuasiQuoter { quoteExp = stringE }
