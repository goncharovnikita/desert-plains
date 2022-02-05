module Html.BodyTags
    ( BodyTag(..)
    , SectionTag(..)
    , GroupingTag(..)
    , TextTag(..)
    , EditTag(..)
    ) where

import ClassyPrelude
import Html.Attribute (Attribute)

data BodyTag
    = SectionTag SectionTag [Attribute] [BodyTag]
    | GroupingTag GroupingTag [Attribute] [BodyTag]
    | TextTag TextTag [Attribute] [BodyTag]
    | EditTag EditTag [Attribute] [BodyTag]
    | PlainText Text
    deriving (Show, Eq, Ord)

-- https://html.spec.whatwg.org/multipage/sections.html
data SectionTag
    = Article
    | Section
    | Nav
    | Aside
    | H1
    | H2
    | H3
    | H4
    | H5
    | H6
    | Hgroup
    | Header
    | Footer
    | Address
    deriving (Show, Eq, Ord)

-- https://html.spec.whatwg.org/multipage/grouping-content.html
data GroupingTag
    = P
    | Hr
    | Pre
    | Blockquote
    | Ol
    | Ul
    | Menu
    | Li
    | Dl
    | Dt
    | Dd
    | Figure
    | Figcaption
    | Main
    | Div
    deriving (Show, Eq, Ord)

-- https://html.spec.whatwg.org/multipage/text-level-semantics.html
data TextTag
    = A
    | Em
    | Strong
    | Small
    | S
    | Cite
    | Q
    | Dfn
    | Abbr
    | Ruby
    | Rt
    | Rp
    | Data
    | Time
    | Code
    | Var
    | Samp
    | Kbd
    | Sub
    | Sup
    | I
    | B
    | U
    | Mark
    | Bdi
    | Bdo
    | Span
    | Br
    | Wbr
    deriving (Show, Eq, Ord)

-- https://html.spec.whatwg.org/multipage/edits.html
data EditTag
    = Ins
    | Del
    deriving (Show, Eq, Ord)
