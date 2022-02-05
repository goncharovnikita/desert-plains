module Html.HeadTags (
    HeadTag(..),
) where

import Html.Attribute
import ClassyPrelude
import Html.Style (Style)

-- https://html.spec.whatwg.org/multipage/semantics.html
data HeadTag
    = Title Text
    | Base [Attribute]
    | Link [Attribute]
    | Meta [Attribute]
    | Style Style
    deriving (Show, Eq, Ord)
