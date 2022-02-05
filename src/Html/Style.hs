module Html.Style (Style(..)) where

import ClassyPrelude

data Style
    = Style [Style]
    | StyleElement Text [Style]
    | StyleProperty (Text, Text)
    deriving (Show, Eq, Ord)
