module HtmlRendererSpec where

import Test.Hspec
import ClassyPrelude
import HtmlRenderer (render)
import Html.HeadTags (HeadTag(..))
import Html.Attribute (Attribute(BasicAttribute))
import qualified Html.HeadTags as Html
import qualified Html.Style as Html

spec :: Spec
spec = describe "HtmlRendererSpec" $ do
    describe "render Head" $ do
        it "should render Title" $
            render (Title "hello") `shouldBe` "<title>hello</title>"
        it "should render Base" $
            render (Base [BasicAttribute ("href", "https://example.com")])
                `shouldBe`
                "<base href=\"https://example.com\" />"
        it "should render Link" $
            render (Link [BasicAttribute ("href", "https://example.com")])
                `shouldBe`
                "<link href=\"https://example.com\" />"
        it "should render Meta" $
            render (Meta [BasicAttribute ("content", "text/html")])
                `shouldBe`
                "<meta content=\"text/html\" />"
        it "should render Style" $
            render (Style $ Html.StyleElement ".body" [Html.StyleProperty ("padding", "12px")] )
                `shouldBe`
                "<style>.body {padding: 12px;}</style>"
