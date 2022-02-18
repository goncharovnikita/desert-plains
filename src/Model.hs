module Model where

import ClassyPrelude
import Html.Html (Html)

newtype Template = Template Html
    deriving (Show, Eq)

----- Startup Models

data Arg
    = Src Text
    | Dest Text
    | Watch
    deriving (Eq, Show)
