module Heredoc (str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

str = QuasiQuoter {
    quoteExp = stringE,
    -- From https://hackage.haskell.org/package/authoring-0.3.3.1/docs/src/Text-Authoring-TH.html
    quotePat  = error "Authoring QuasiQuotes are only for expression context",
    quoteType = error "Authoring QuasiQuotes are only for expression context",
    quoteDec  = error "Authoring QuasiQuotes are only for expression context"
}
