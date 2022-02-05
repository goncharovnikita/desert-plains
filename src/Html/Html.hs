module Html.Html (Html(..)) where

import Html.Head
import Html.Body
import ClassyPrelude

data Html
    = Html Head Body
    deriving (Show, Eq, Ord)
