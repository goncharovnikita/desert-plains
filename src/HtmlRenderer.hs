module HtmlRenderer
    ( render
    ) where


import ClassyPrelude
import qualified Html.Style
import qualified Html.HeadTags
import Html.Head (Head(..))
import Html.Body (Body(..))
import Html.Html (Html(..))
import Html.Attribute (Attribute(..))
import Html.HeadTags
import Html.BodyTags

class Renderable a where
    render :: a -> Text

instance Renderable Text where
    render = id

instance Renderable Html.Style.Style where
    render template = case template of
        Html.Style.StyleElement elementName children ->
            concat [elementName, " {", concatMap render children, "}"]
        Html.Style.StyleProperty (propName, propValue) ->
            concat [propName, ": ", propValue, ";"]

instance Renderable Head where
    render template = case template of
        Head attrs children -> makeTagWithAttributes "head" attrs children

instance Renderable Body where
    render template = case template of
        Body attrs children -> makeTagWithAttributes "body" attrs children

instance Renderable Html where
    render (Html h b) = makeTag "html" [render h, render b]

instance Renderable HeadTag where
    render headTag = case headTag of
        Title t -> makeTag "title" [t]
        Base attrs -> makeSelfClosingTag "base" attrs
        Link attrs -> makeSelfClosingTag "link" attrs
        Meta attrs -> makeSelfClosingTag "meta" attrs
        Style style -> makeTag "style" [style]

instance Renderable BodyTag where
    render bodyTag = case bodyTag of
        _ -> ""

makeTag :: (Renderable a) => Text -> [a] -> Text
makeTag tagName child = concat ["<", tagName, ">", children, "</", tagName, ">"]
    where children = concatMap render child

makeTagWithAttributes :: (Renderable a) => Text -> [Attribute] -> [a] -> Text
makeTagWithAttributes tagName attrs child = concat ["<", tagName, " ", renderAttributes attrs, ">", children, "</", tagName, ">"]
    where children = concatMap render child

makeSelfClosingTag :: Text -> [Attribute] -> Text
makeSelfClosingTag tagName attrs = concat ["<", tagName, " ", renderAttributes attrs, " />"]

renderAttributes :: [Attribute] -> Text
renderAttributes = unwords . map renderAttribute

renderAttribute :: Attribute -> Text
renderAttribute attr = case attr of
    ClassList classes -> concat ["class=\"", unwords classes, "\""]
    BasicAttribute (attrName, attrValue) -> concat [attrName, "=\"", attrValue, "\""]
    ID _ -> ""

renderHeadScript :: [Attribute] -> Text
renderHeadScript attributes = makeTagWithAttributes "script" attributes emptyChildren
    where emptyChildren :: [Head]
          emptyChildren = []
