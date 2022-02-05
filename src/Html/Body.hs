module Html.Body (
    Body(..),
) where

import Html.Attribute
import Html.BodyTags
import ClassyPrelude

data Body = Body [Attribute] [BodyTag]
    deriving (Show, Eq, Ord)
