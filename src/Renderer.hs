module Renderer
    ( render
    ) where


import ClassyPrelude
import Model

class Renderable a where
    render :: a -> Text

instance Renderable InterpolatedString where
    render (InterpolatedString children) = concat ["`", concatMap render children, "`"]
    render (InterpolatedText txt) = txt
    render (InterpolatedValue val) = concat ["${", val, "}"]

instance Renderable Text where
    render = id

instance Renderable StyleFamily where
    render template = case template of
        Style children -> makeSimpleTag "style" children
        StyleElement elementName children ->
            concat [elementName, " {", concatMap render children, "}"]
        StyleProperty (propName, propValue) ->
            concat [propName, ": ", propValue, ";"]

instance Renderable HeadNodeFamily where
    render template = case template of
        HeadNode children -> makeSimpleTag "head" children
        Title txt -> makeSimpleTag "title" [txt]
        StyleNode styleNode -> render styleNode
        SelfContainedScriptNode attrs -> renderHeadScript attrs


instance Renderable BodyNodeFamily where
    render template = case template of
        (BodyNode attrs tmpl) -> makeTagWithAttributes "body" attrs tmpl
        (Div attrs tmpl) -> makeTagWithAttributes "div" attrs tmpl
        (H1 attrs tmpl) -> makeTagWithAttributes "h1" attrs tmpl
        (A attrs tmpl) -> makeTagWithAttributes "a" attrs tmpl
        (Button attrs tmpl) -> makeTagWithAttributes "button" attrs tmpl
        (TextNode txt) -> txt
        (ComponentExpression txt _ _) -> txt
        (BodyInterpolatedString txt) -> render txt

instance Renderable Template where
    render (Html h b _) = makeSimpleTag "html" [render h, render b]

makeSimpleTag :: (Renderable a) => Text -> [a] -> Text
makeSimpleTag tagName child = concat ["<", tagName, ">", children, "</", tagName, ">"]
    where children = concatMap render child

makeTagWithAttributes :: (Renderable a) => Text -> [Attribute] -> [a] -> Text
makeTagWithAttributes tagName attrs child = concat ["<", tagName, " ", renderAttributes attrs, ">", children, "</", tagName, ">"]
    where children = concatMap render child

renderAttributes :: [Attribute] -> Text
renderAttributes = unwords . map renderAttribute

renderAttribute :: Attribute -> Text
renderAttribute attr = case attr of
    ClassList classes -> concat ["class=\"", unwords classes, "\""]
    BasicAttribute (attrName, attrValue) -> concat [attrName, "=\"", attrValue, "\""]
    ID _ -> ""
    InterpolatedAttribute _ -> ""
    BoolAttribute _ -> ""

renderHeadScript :: [Attribute] -> Text
renderHeadScript attributes = makeTagWithAttributes "script" attributes emptyChildren
    where emptyChildren :: [HeadNodeFamily]
          emptyChildren = []