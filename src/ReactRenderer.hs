module ReactRenderer (render) where

import ClassyPrelude
import Model

class Renderable a where
    render :: a -> Text

class ReactRenderable a where
    renderReact :: [Component] -> a -> Text

instance ReactRenderable Text where
    renderReact _ a = a

instance Renderable Text where
    render = id

instance Renderable InterpolatedString where
    render (InterpolatedString children) = concat ["`", concatMap render children, "`"]
    render (InterpolatedText txt) = txt
    render (InterpolatedValue val) = concat ["${", val, "}"]

instance Renderable StyleFamily where
    render template = case template of
        Style children -> makeSimpleTag "style" children
        StyleElement elementName children ->
            concat [elementName, " {", concatMap render children, "}"]
        StyleProperty (propName, propValue) ->
            concat [propName, ": ", propValue, ";"]

instance Renderable HeadNodeFamily where
    render template = case template of
        HeadNode children -> renderHead children
        Title txt -> makeSimpleTag "title" [txt]
        StyleNode styleNode -> render styleNode
        SelfContainedScriptNode attributes -> renderHeadScript attributes


instance ReactRenderable BodyNodeFamily where
    renderReact comps template = case template of
        (BodyNode attrs tmpl) -> renderBody attrs tmpl comps
        (Div attrs tmpl) -> makeReactTag "div" attrs tmpl comps
        (H1 attrs tmpl) -> makeReactTag "h1" attrs tmpl comps
        (A attrs tmpl) -> makeReactTag "a" attrs tmpl comps
        (Button attrs tmpl) -> makeReactTag "button" attrs tmpl comps
        (TextNode txt) -> concat ["`", txt, "`"]
        (ComponentExpression name attrs children) -> makeReactTagHelper name attrs children comps
        (BodyInterpolatedString str) -> render str

instance Renderable Template where
    render (Html h b c) = makeSimpleTag "html" [render h, renderReact c b]

----- HEAD RENDERERS

renderHead :: [HeadNodeFamily] -> Text
renderHead children = makeSimpleTag "head" renderHeadChildren
    where renderHeadChildren = children
            <> [ SelfContainedScriptNode [BoolAttribute "crossorigin", BasicAttribute ("src", "https://unpkg.com/react@17/umd/react.development.js")]
               , SelfContainedScriptNode [BoolAttribute "crossorigin", BasicAttribute ("src", "https://unpkg.com/react-dom@17/umd/react-dom.development.js")]
               ]

renderHeadScript :: [Attribute] -> Text
renderHeadScript attributes = makeTagWithAttributes "script" attributes emptyChildren
    where emptyChildren :: [HeadNodeFamily]
          emptyChildren = []

----- BODY RENDERERS

renderBody :: [Attribute] -> [BodyNodeFamily] -> [Component] -> Text
renderBody attrs children comps = makeTagWithAttributes "body" attrs renderBodyChildren
    where renderBodyChildren =
            [ makeEmptyTagWithAttributes "div" [ID "app"]
            , renderBodyScript children comps
            ]

renderBodyScript :: [BodyNodeFamily] -> [Component] -> Text
renderBodyScript children comps = makeSimpleTag "script" bodyScriptContent
    where bodyScriptContent :: [Text]
          bodyScriptContent = [concat
            [ "const {useState,useEffect,useReducer,useCallback,createElement:ce}=React;\n"
            , renderComponentsDeclarations comps
            , "ReactDOM.render(", intercalate "," (map (renderReact comps) children), ","
            , "document.getElementById('app')", ")"
            ]]

---- REACT RENDERERS

renderComponentsDeclarations :: [Component] -> Text
renderComponentsDeclarations comps = intercalate ";\n" (map renderComponentDeclaration comps)
    where renderComponentDeclaration (Component name props logic tmpl) = concat
            [ "const ", name, " = ", renderProps props, " => {\n"
            , concat logic
            , "return ce(React.Fragment, null, "
            , intercalate ", " (map (renderReact comps) tmpl)
            , ")\n}\n"
            ]

renderProps :: [Text] -> Text
renderProps props = concat ["({children,", content, "})"]
    where content = if null props then "...props" else intercalate "," props

---- RENDER HELPERS

makeSimpleReactTag :: Text -> [BodyNodeFamily] -> [Component] -> Text
makeSimpleReactTag name = makeReactTag name []

makeReactTag :: Text -> [Attribute] -> [BodyNodeFamily] -> [Component] -> Text
makeReactTag name = makeReactTagHelper (concat ["\"", name, "\""])

makeReactTagHelper :: Text -> [Attribute] -> [BodyNodeFamily] -> [Component] -> Text
makeReactTagHelper name attrs children comps = concat
    [ "ce(", name, ","
    , renderAttributesAsProps attrs, ","
    , "", intercalate "," (map (renderReact comps) children), "", ")"
    ]

renderAttributesAsProps :: [Attribute] -> Text
renderAttributesAsProps attrs = concat
    [ "{",
      intercalate "," (map renderAttributeAsProp attrs),
      "}"
    ]

renderAttributeAsProp :: Attribute -> Text
renderAttributeAsProp attr = case attr of
    ClassList classes -> concat ["className: \"", unwords classes, "\""]
    ID idName -> concat ["id: \"", idName, "\""]
    BoolAttribute attrName -> attrName ++ ": true"
    BasicAttribute (attrName, attrValue) -> concat [attrName, ": \"", attrValue, "\""]
    InterpolatedAttribute (attrName, attrValue) -> concat [attrName, ": ", attrValue]

-- makeRaw

makeSimpleTag :: (Renderable a) => Text -> [a] -> Text
makeSimpleTag tagName child = concat ["<", tagName, ">", children, "</", tagName, ">"]
    where children = concatMap render child

makeEmptyTagWithAttributes :: Text -> [Attribute] -> Text
makeEmptyTagWithAttributes tagName attrs = concat ["<", tagName, " ", renderAttributes attrs, ">", "</", tagName, ">"]

makeTagWithAttributes :: (Renderable a) => Text -> [Attribute] -> [a] -> Text
makeTagWithAttributes tagName attrs child = concat ["<", tagName, " ", renderAttributes attrs, ">", children, "</", tagName, ">"]
    where children = concatMap render child

renderAttributes :: [Attribute] -> Text
renderAttributes = unwords . map renderAttribute

renderAttribute :: Attribute -> Text
renderAttribute attr = case attr of
    ClassList classes -> concat ["class=\"", unwords classes, "\""]
    ID idName -> concat ["id=\"", idName, "\""]
    BasicAttribute (attrName, attrValue) -> concat [attrName, "=\"", attrValue, "\""]
    BoolAttribute attrName -> attrName

----- HELPERS
