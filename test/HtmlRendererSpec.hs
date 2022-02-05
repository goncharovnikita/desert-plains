module HtmlRendererSpec where

import Test.Hspec
import ClassyPrelude
import HtmlRenderer (render)
import Html.HeadTags (HeadTag(..))
import Html.Attribute (Attribute(BasicAttribute))

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
