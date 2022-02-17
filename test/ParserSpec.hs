module ParserSpec where

import ClassyPrelude
import Test.Hspec
import Parser
import Model
import Data.Char (isUpper)
import Html.Attribute
import Html.Style
import Html.HeadTags
import Html.BodyTags
import Html.Head (Head(Head))
import Html.Body (Body(Body))

tt = execTemplateParserForResult

spec :: Spec
spec = describe "ParserSpec" $ do
    describe "parseChar" $ do
        it "should parse char" $
            tt (parseChar 'h') "hello, world!" `shouldBe` Right 'h'
    describe "parseText" $ do
        it "should parse text" $
            tt (parseText "hello") "hello, world!" `shouldBe` Right "hello"
    describe "ws" $ do
        it "should parse single whitespace" $
            tt ws " hello, world!" `shouldBe` Right " "
        it "should parse long whitespace" $
            tt ws "   hello, world!" `shouldBe` Right "   "
    describe "wsOrNewline" $ do
        it "should parse single whitespace" $
            tt ws " hello, world!" `shouldBe` Right " "
        it "should parse long whitespace" $
            tt ws "   hello, world!" `shouldBe` Right "   "
        it "should parse whitespace after newline" $
            tt ws "\n hello, world!" `shouldBe` Right "\n "
        it "should parse newline in long whitespace" $
            tt ws " \n  hello, world!" `shouldBe` Right " \n  "
    describe "spanP" $ do
        it "should parse text until it reach newline" $
            tt (spanP (/='\n')) "hello,\n world!" `shouldBe` Right "hello,"
    describe "parseBasicAttribute" $ do
        it "should parse basic attribute" $
            tt parseBasicAttribute "name=\"someName\"" `shouldBe` Right (BasicAttribute ("name", "someName"))
    describe "parseClass" $ do
        it "should parse class" $
            tt parseClass ".btn" `shouldBe` Right "btn"
    describe "parseAllClasses" $ do
        it "should parse all classes" $
            tt parseAllClasses ".btn.primary" `shouldBe` Right (ClassList ["btn", "primary"])
    describe "parseCharCond" $ do
        it "should parse capitalized char" $
            tt (parseCharCond isUpper) "Component" `shouldBe` Right 'C'
    describe "parseTextWithPrefix" $ do
        it "should parse text with prefix" $
            tt (parseTextWithPrefix "." (spanP (const True))) ".class" `shouldBe` Right ".class"

    -- Head parsers
    describe "Head parsers" $ do
        it "should parse title" $
            tt
                parseTitle "[title \"some title\"]"
            `shouldBe`
            Right (Title "some title")
        it "should parse base" $
            tt
                parseBase "[base href=\"https://example.com\"]"
            `shouldBe`
            Right (Base [BasicAttribute ("href", "https://example.com")])
        it "should parse link" $
            tt
                parseLink "[link href=\"https://example.com\"]"
            `shouldBe`
            Right (Link [BasicAttribute ("href", "https://example.com")])
        it "should parse meta" $
            tt
                parseMeta "[meta href=\"https://example.com\"]"
            `shouldBe`
            Right (Meta [BasicAttribute ("href", "https://example.com")])
        describe "Style parsers" $ do
            it "should parse style property" $
                tt parseStyleProperty "font-size: 1rem;"
                `shouldBe`
                Right (StyleProperty ("font-size", "1rem"))
            it "should parse style contents" $
                tt parseStyleContents "[div] [.btn] [#root]"
                `shouldBe`
                Right [
                    StyleElement "div" [],
                    StyleElement ".btn" [],
                    StyleElement "#root" []
                ]
            it "should parse style contents with data" $
                tt parseStyleContents "[div\nmargin: 0;\n]\n[.btn\nfont-size: 16px;\n]\n[#root\npadding: 12px;\n]"
                `shouldBe`
                Right [
                    StyleElement "div" [StyleProperty ("margin", "0")],
                    StyleElement ".btn" [StyleProperty ("font-size", "16px")],
                    StyleElement "#root" [StyleProperty ("padding", "12px")]
                ]
            it "should parse style" $
                tt
                    parseHeadStyle "[style\n[body\nmargin: 0;\n]\n[.block\nfont-size: 16px;\n]\n[#id padding: 12px;\n]\n]"
                `shouldBe`
                Right (Style [
                    StyleElement "body" [StyleProperty ("margin", "0")],
                    StyleElement ".block" [StyleProperty ("font-size", "16px")],
                    StyleElement "#id" [StyleProperty ("padding", "12px")]
                ])
        it "should parse head tag" $
            tt 
                parseHead "\
                \[head\n\
                    \[title \"Application\"]\n\
                    \[meta description=\"Basic application\"]\n\
                    \[base href=\"https://application.com\"]\n\
                    \[link href=\"https://application.com\"]\n\
                    \[style\n\
                        \[.div\n\
                            \font-size: 12px;\n\
                        \]\n\
                    \]\n\
                \]\n\
                \"
            `shouldBe`
            Right (Head [] [
                Title "Application",
                Meta [BasicAttribute ("description", "Basic application")],
                Base [BasicAttribute ("href", "https://application.com")],
                Link [BasicAttribute ("href", "https://application.com")],
                Style [
                    StyleElement ".div" [StyleProperty ("font-size", "12px")]
                ]
            ])
    

    -- Body parsers
    describe "Body parsers" $ do
        it "should parse PlainText" $
            tt parseBodyContentItem "\"hello\""
            `shouldBe`
            Right (PlainText "hello")
        mapM_ (\(a, tag) -> do
            let markup = mconcat [asText "[", tag, asText " ", asText "\"", tag, asText "\"", asText "\n]"]
            it ("should parse " <> show tag <> " " <> show markup) $
                tt parseBodyContentItem markup
                `shouldBe`
                Right (SectionTag a [] [PlainText tag])
            ) [
                (Article, asText "article"),
                (Section, asText "section"),
                (Nav, asText "nav"),
                (Aside, asText "aside"),
                (H1, asText "h1"),
                (H2, asText "h2"),
                (H3, asText "h3"),
                (H4, asText "h4"),
                (H5, asText "h5"),
                (H6, asText "h6"),
                (Hgroup, asText "hgroup"),
                (Header, asText "header"),
                (Footer, asText "footer"),
                (Address, asText "address")
            ]
        mapM_ (\(a, tag) -> do
            let markup = mconcat [asText "[", tag, asText " ", asText "\"", tag, asText "\"", asText "\n]"]
            it ("should parse " <> show tag <> " " <> show markup) $
                tt parseBodyContentItem markup
                `shouldBe`
                Right (GroupingTag a [] [PlainText tag])
            ) [
                (P, asText "p"),
                (Hr, asText "hr"),
                (Pre, asText "pre"),
                (Blockquote, asText "blockquote"),
                (Ol, asText "ol"),
                (Ul, asText "ul"),
                (Menu, asText "menu"),
                (Li, asText "li"),
                (Dl, asText "dl"),
                (Dt, asText "dt"),
                (Dd, asText "dd"),
                (Figure, asText "figure"),
                (Figcaption, asText "figcaption"),
                (Main, asText "main"),
                (Div, asText "div")
            ]
        mapM_ (\(a, tag) -> do
            let markup = mconcat [asText "[", tag, asText " ", asText "\"", tag, asText "\"", asText "\n]"]
            it ("should parse " <> show tag <> " " <> show markup) $
                tt parseBodyContentItem markup
                `shouldBe`
                Right (TextTag a [] [PlainText tag])
            ) [
                (A, asText "a"),
                (Em, asText "em"),
                (Strong, asText "strong"),
                (Small, asText "small"),
                (S, asText "s"),
                (Cite, asText "cite"),
                (Q, asText "q"),
                (Dfn, asText "dfn"),
                (Abbr, asText "abbr"),
                (Ruby, asText "ruby"),
                (Rt, asText "rt"),
                (Rp, asText "rp"),
                (Data, asText "data"),
                (Time, asText "time"),
                (Code, asText "code"),
                (Var, asText "var"),
                (Samp, asText "samp"),
                (Kbd, asText "kbd"),
                (Sub, asText "sub"),
                (Sup, asText "sup"),
                (I, asText "i"),
                (B, asText "b"),
                (U, asText "u"),
                (Mark, asText "mark"),
                (Bdi, asText "bdi"),
                (Bdo, asText "bdo"),
                (Span, asText "span"),
                (Br, asText "br"),
                (Wbr, asText "wbr")
            ]
        mapM_ (\(a, tag) -> do
            let markup = mconcat [asText "[", tag, asText " ", asText "\"", tag, asText "\"", asText "\n]"]
            it ("should parse " <> show tag <> " " <> show markup) $
                tt parseBodyContentItem markup
                `shouldBe`
                Right (EditTag a [] [PlainText tag])
            ) [
                (Ins, asText "ins"),
                (Del, asText "del")
            ]

