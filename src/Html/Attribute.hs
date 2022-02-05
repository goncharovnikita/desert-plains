module Html.Attribute (
    Attribute(..),
) where

import ClassyPrelude

data Attribute
    = ClassList [Text]
    | ID Text
    | BasicAttribute (Text, Text)
    deriving (Show, Eq, Ord)
