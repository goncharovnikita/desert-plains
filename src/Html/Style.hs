module Html.Style (Style(..)) where

import ClassyPrelude

data Style
    = StyleElement Text [Style]
    | StyleProperty (Text, Text)
    deriving (Show, Eq, Ord)
