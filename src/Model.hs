module Model where

import ClassyPrelude

data InterpolatedString
    = InterpolatedString [InterpolatedString]
    | InterpolatedText Text
    | InterpolatedValue Text
    deriving (Show, Eq)

data StyleFamily
    = Style [StyleFamily]
    | StyleElement Text [StyleFamily]
    | StyleProperty (Text, Text)
    deriving (Show, Eq)

data Attribute
    = ClassList [Text]
    | ID Text
    | BasicAttribute (Text, Text)
    | InterpolatedAttribute (Text, Text)
    | BoolAttribute Text
    deriving (Show, Eq)

data HeadNodeFamily
    = HeadNode [HeadNodeFamily]
    | Title Text
    | StyleNode StyleFamily
    | SelfContainedScriptNode [Attribute]
    deriving (Show, Eq)

data BodyNodeFamily
    = BodyNode [Attribute] [BodyNodeFamily]
    | Div [Attribute] [BodyNodeFamily]
    | H1 [Attribute] [BodyNodeFamily]
    | A [Attribute] [BodyNodeFamily]
    | Button [Attribute] [BodyNodeFamily]
    | TextNode Text
    | ComponentExpression Text [Attribute] [BodyNodeFamily]
    | BodyInterpolatedString InterpolatedString
    deriving (Show, Eq)

data Component = Component
    { componentName :: Text
    , componentProps :: [Text]
    , componentLogic :: [Text]
    , componentRender :: [BodyNodeFamily]
    } deriving (Show, Eq)

data Template = Html HeadNodeFamily BodyNodeFamily [Component]
    deriving (Show, Eq)

----- Startup Models

data Arg
    = Src Text
    | Dest Text
    | Watch
    deriving (Eq, Show)
