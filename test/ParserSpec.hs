module ParserSpec where

import ClassyPrelude
import Test.Hspec
import Parser
import Model
import Data.Char (isUpper)

spec :: Spec
spec = describe "ParserSpec" $ do
    describe "parseChar" $ do
        it "should parse char" $
            execTemplateParserForResult (parseChar 'h') "hello, world!" `shouldBe` Right 'h'
    describe "parseText" $ do
        it "should parse text" $
            execTemplateParserForResult (parseText "hello") "hello, world!" `shouldBe` Right "hello"
    describe "ws" $ do
        it "should parse single whitespace" $
            execTemplateParserForResult ws " hello, world!" `shouldBe` Right " "
        it "should parse long whitespace" $
            execTemplateParserForResult ws "   hello, world!" `shouldBe` Right "   "
    describe "wsOrNewline" $ do
        it "should parse single whitespace" $
            execTemplateParserForResult ws " hello, world!" `shouldBe` Right " "
        it "should parse long whitespace" $
            execTemplateParserForResult ws "   hello, world!" `shouldBe` Right "   "
        it "should parse whitespace after newline" $
            execTemplateParserForResult ws "\n hello, world!" `shouldBe` Right "\n "
        it "should parse newline in long whitespace" $
            execTemplateParserForResult ws " \n  hello, world!" `shouldBe` Right " \n  "
    describe "spanP" $ do
        it "should parse text until it reach newline" $
            execTemplateParserForResult (spanP (/='\n')) "hello,\n world!" `shouldBe` Right "hello,"
    describe "parseBasicAttribute" $ do
        it "should parse basic attribute" $
            execTemplateParserForResult parseBasicAttribute "name=\"someName\"" `shouldBe` Right (BasicAttribute ("name", "someName"))
    describe "parseBoolAttribute" $ do
        it "should parse bool attribute" $
            execTemplateParserForResult parseBoolAttribute "rounded " `shouldBe` Right (BoolAttribute "rounded")
    describe "parseInterpolatedAttribute" $ do
        it "should parse interpolated attribute" $
            execTemplateParserForResult parseInterpolatedAttribute "name={someName}" `shouldBe` Right (InterpolatedAttribute ("name", "someName"))
    describe "parseClass" $ do
        it "should parse class" $
            execTemplateParserForResult parseClass ".btn" `shouldBe` Right "btn"
    describe "parseAllClasses" $ do
        it "should parse all classes" $
            execTemplateParserForResult parseAllClasses ".btn.primary" `shouldBe` Right (ClassList ["btn", "primary"])
    describe "parseAttributes" $ do
        it "should parse all attributes" $
            execTemplateParserForResult parseAttributes ".btn.primary href=\"/about\" name={someName}" `shouldBe` Right [ClassList ["btn", "primary"], BasicAttribute ("href", "/about"), InterpolatedAttribute ("name", "someName")]
    describe "parseCharCond" $ do
        it "should parse capitalized char" $
            execTemplateParserForResult (parseCharCond isUpper) "Component" `shouldBe` Right 'C'
    describe "parseStyleProperty" $ do
        it "should parse style property" $
            execTemplateParserForResult parseStyleProperty "font-size: 1rem;" `shouldBe` Right (StyleProperty ("font-size", "1rem"))
    describe "parseTextWithPrefix" $ do
        it "should parse text with prefix" $
            execTemplateParserForResult (parseTextWithPrefix "." (spanP (const True))) ".class" `shouldBe` Right ".class"
    describe "parseStyleContents" $ do
        it "should parse style contents" $
            execTemplateParserForResult parseStyleContents "[div] [.btn] [#root]" `shouldBe` Right [StyleElement "div" [], StyleElement ".btn" [], StyleElement "#root" []]
    describe "parseStyleNode" $ do
        it "should parse style node" $
            execTemplateParserForResult parseStyleNode "[style [div] [.btn] [#root]]" `shouldBe` Right (Style [StyleElement "div" [], StyleElement ".btn" [], StyleElement "#root" []])
    describe "interpolatedStringLiteral" $ do
        it "should parse interpolated string literal text" $
            execTemplateParserForResult interpolatedStringLiteral "Hello, " `shouldBe` Right (InterpolatedText "Hello, ")
        it "should parse interpolated string literal text without '}'" $
            execTemplateParserForResult interpolatedStringLiteral "Hello, }" `shouldBe` Right (InterpolatedText "Hello, ")
        it "should parse interpolated string literal value" $
            execTemplateParserForResult interpolatedStringLiteral "#name" `shouldBe` Right (InterpolatedValue "name")
        it "should parse interpolated string literal value without '}'" $
            execTemplateParserForResult interpolatedStringLiteral "#name}" `shouldBe` Right (InterpolatedValue "name")
        it "should parse interpolated string literal value without ' '" $
            execTemplateParserForResult interpolatedStringLiteral "#name " `shouldBe` Right (InterpolatedValue "name")
    describe "parseInterpolatedString" $ do
        it "should parse interpolated string" $
            execTemplateParserForResult parseInterpolatedString "{Hello, #name}" `shouldBe` Right (InterpolatedString [InterpolatedText "Hello, ", InterpolatedValue "name"])
    describe "parseBodyContentItem" $ do
        it "should parse body content item" $
            execTemplateParserForResult parseBodyContentItem "{Hello, #name}" `shouldBe` Right (BodyInterpolatedString $ InterpolatedString [InterpolatedText "Hello, ", InterpolatedValue "name"])
    describe "parseBodyContents" $ do
        it "should parse body contents" $
            execTemplateParserForResult parseBodyContents "[h1 {Hello, #name}]" `shouldBe` Right ([H1 [] [BodyInterpolatedString $ InterpolatedString [InterpolatedText "Hello, ", InterpolatedValue "name"]]])
    describe "parseComponentLogic" $ do
        it "should parse component logic" $
            execTemplateParserForResult parseComponentLogic "{{\nconst [s, cs] = useState(0);\nlet a = ''\n}}" `shouldBe` Right ["const [s, cs] = useState(0);\nlet a = ''\n"]
    describe "parseComponentProps" $ do
        it "should parse component props" $
            execTemplateParserForResult parseComponentProps "[name, surname]" `shouldBe` Right ["name", "surname"]
    describe "parseComponent" $ do
        it "should parse component" $
            execTemplateParserForResult parseComponent "[Welcommer[name] [h1 {Hello, #name}]]" `shouldBe` Right (Component "Welcommer" ["name"] [] [H1 [] [BodyInterpolatedString $ InterpolatedString [InterpolatedText "Hello, ", InterpolatedValue "name"]]])
    describe "parseComponents" $ do
        it "should parse components" $
            execTemplateParserForResult parseComponents "[Halloyer {{const a = 'hallo'}} \"Hallo!\"]\n[Welcommer[name,surname] [h1 {Hello, #name}]]"
                `shouldBe`
                Right
                    [ Component "Halloyer" [] ["const a = 'hallo'"] [TextNode "Hallo!"]
                    , Component "Welcommer" ["name", "surname"] [] [H1 [] [BodyInterpolatedString $ InterpolatedString [InterpolatedText "Hello, ", InterpolatedValue "name"]]]
                    ]
    
