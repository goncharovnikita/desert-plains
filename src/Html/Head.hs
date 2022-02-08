module Html.Head (
    Head(..),
) where

import Html.Attribute
import Html.HeadTags
import ClassyPrelude

data Head = Head [Attribute] [HeadTag]
    deriving (Show, Eq, Ord)
