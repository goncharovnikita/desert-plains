module HelpersSpec where

import ClassyPrelude
import Test.Hspec
import Helpers

spec :: Spec
spec = describe "HelpersSpec" $ do
    it "(longSpan) truncates text into two" $
        longSpan 2 (/="}}") "hello}}world!" `shouldBe` ("hello", "}}world!")
    it "(longSpan) should not truncate text" $
        longSpan 2 (/="}}") "hello world!" `shouldBe` ("hello world!", "")

