module Component where

import ClassyPrelude
import Html.Html (Html)
import Html.BodyTags (BodyTag)

data InterpolatedString
    = InterpolatedString [InterpolatedString]
    | InterpolatedText Text
    | InterpolatedValue Text
    deriving (Show, Eq)

data ComponentRender
    = ComponentBody [BodyTag]
    | BodyInterpolatedString InterpolatedString
    deriving (Show, Eq)

data Component = Component
    { componentName :: Text
    , componentProps :: [Text]
    , componentLogic :: [Text]
    , componentRender :: [ComponentRender]
    } deriving (Show, Eq)