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

class TagNameHolder a where
    getTagName :: a -> Text

instance Renderable Text where
    render = id

instance TagNameHolder Text where
    getTagName = id

instance Renderable Html.Style.Style where
    render template = case template of
        Html.Style.StyleElement elementName children ->
            concat [elementName, " {", concatMap render children, "}"]
        Html.Style.StyleProperty (propName, propValue) ->
            concat [propName, ": ", propValue, ";"]

instance Renderable Head where
    render template = case template of
        Head attrs children -> makeTagWithAttributes (asText "head") attrs children

instance Renderable Body where
    render template = case template of
        Body attrs children -> makeTagWithAttributes (asText "body") attrs children

instance Renderable Html where
    render (Html h b) = makeTag (asText "html")[render h, render b]

instance Renderable HeadTag where
    render headTag = case headTag of
        Title t -> makeTag (asText "title") [t]
        Base attrs -> makeSelfClosingTag "base" attrs
        Link attrs -> makeSelfClosingTag "link" attrs
        Meta attrs -> makeSelfClosingTag "meta" attrs
        Style style -> makeTag (asText "style") style

instance Renderable BodyTag where
    render bodyTag = case bodyTag of
        SectionTag st attrs children -> makeTagWithAttributes st attrs children
        GroupingTag gt attrs children -> makeTagWithAttributes gt attrs children
        TextTag tt attrs children -> makeTagWithAttributes tt attrs children
        EditTag et attrs children -> makeTagWithAttributes et attrs children
        PlainText t -> render t

instance TagNameHolder SectionTag where
    getTagName sectionTag = case sectionTag of
        Article -> "article"
        Section -> "section"
        Nav -> "nav"
        Aside -> "aside"
        H1 -> "h1"
        H2 -> "h2"
        H3 -> "h3"
        H4 -> "h4"
        H5 -> "h5"
        H6 -> "h6"
        Hgroup -> "hgroup"
        Header -> "header"
        Footer -> "footer"
        Address -> "address"

instance TagNameHolder GroupingTag where
    getTagName groupingTag = case groupingTag of
        P -> "p"
        Hr -> "hr"
        Pre -> "pre"
        Blockquote -> "blockquote"
        Ol-> "ol"
        Ul-> "ul"
        Menu-> "menu"
        Li-> "li"
        Dl-> "dl"
        Dt-> "dt"
        Dd-> "dd"
        Figure-> "figure"
        Figcaption-> "figcaption"
        Main-> "main"
        Div-> "div"

instance TagNameHolder TextTag where
    getTagName textTag = case textTag of
        A-> "a"
        Em-> "em"
        Strong-> "strong"
        Small-> "small"
        S-> "s"
        Cite-> "cite"
        Q-> "q"
        Dfn-> "dfn"
        Abbr-> "abbr"
        Ruby-> "ruby"
        Rt-> "rt"
        Rp-> "rp"
        Data-> "data"
        Time-> "time"
        Code-> "code"
        Var-> "var"
        Samp-> "samp"
        Kbd-> "kbd"
        Sub-> "sub"
        Sup-> "sup"
        I-> "i"
        B-> "b"
        U-> "u"
        Mark-> "mark"
        Bdi-> "bdi"
        Bdo-> "bdo"
        Span-> "span"
        Br-> "br"
        Wbr-> "wbr"

instance TagNameHolder EditTag where
    getTagName editTag = case editTag of
        Ins-> "ins"
        Del-> "del"

makeTag :: (Renderable a, TagNameHolder b) => b -> [a] -> Text
makeTag tagHolder child = concat ["<", tagName, ">", children, "</", tagName, ">"]
    where children = concatMap render child
          tagName = getTagName tagHolder

makeTagWithAttributes :: (Renderable a, TagNameHolder b) => b -> [Attribute] -> [a] -> Text
makeTagWithAttributes tagHolder attrs child = concat ["<", tagName, attrSpacer, renderAttributes attrs, ">", children, "</", tagName, ">"]
    where children = concatMap render child
          tagName = getTagName tagHolder
          attrSpacer = if null attrs then "" else " "

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
renderHeadScript attributes = makeTagWithAttributes (asText "script") attributes emptyChildren
    where emptyChildren :: [Head]
          emptyChildren = []
