module HtmlRendererSpec where

import Test.Hspec
import ClassyPrelude
import HtmlRenderer (render)
import Html.HeadTags (HeadTag(..))
import Html.BodyTags
import Html.Attribute (Attribute(BasicAttribute))
import qualified Html.HeadTags as Html
import qualified Html.Style as Html
import Html.Head (Head(Head))

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
            render (
                Style $ [Html.StyleElement ".body" [Html.StyleProperty ("padding", "12px")]]
            ) `shouldBe`
            "<style>.body {padding: 12px;}</style>"
        it "should render Head with children tags" $
            render (
                Head [] [
                    Title "some title",
                    Base [BasicAttribute ("href", "https://example.com")],
                    Link [BasicAttribute ("href", "https://link.com")],
                    Meta [BasicAttribute ("content", "text/html")],
                    Style $ [
                        Html.StyleElement ".body" [Html.StyleProperty ("padding", "12px")],
                        Html.StyleElement "#h1" [Html.StyleProperty ("font-size", "120em")]
                    ]
                ]
            ) `shouldBe` concat [
                "<head>",
                "<title>some title</title>",
                "<base href=\"https://example.com\" />",
                "<link href=\"https://link.com\" />",
                "<meta content=\"text/html\" />",
                "<style>.body {padding: 12px;}#h1 {font-size: 120em;}</style>",
                "</head>"
            ]
    describe "render Body" $ do
        describe "SectionTag" $ do
            it "should render Article" $
                render (SectionTag Article [] [PlainText "article"]) `shouldBe` "<article>article</article>"
            it "should render Section" $
                render (SectionTag Section [] [PlainText "section"]) `shouldBe` "<section>section</section>"
            it "should render Nav" $
                render (SectionTag Nav [] [PlainText "nav"]) `shouldBe` "<nav>nav</nav>"
            it "should render Aside" $
                render (SectionTag Aside [] [PlainText "aside"]) `shouldBe` "<aside>aside</aside>"
            it "should render H1" $
                render (SectionTag H1 [] [PlainText "h1"]) `shouldBe` "<h1>h1</h1>"
            it "should render H2" $
                render (SectionTag H2 [] [PlainText "h2"]) `shouldBe` "<h2>h2</h2>"
            it "should render H3" $
                render (SectionTag H3 [] [PlainText "h3"]) `shouldBe` "<h3>h3</h3>"
            it "should render H4" $
                render (SectionTag H4 [] [PlainText "h4"]) `shouldBe` "<h4>h4</h4>"
            it "should render H5" $
                render (SectionTag H5 [] [PlainText "h5"]) `shouldBe` "<h5>h5</h5>"
            it "should render H6" $
                render (SectionTag H6 [] [PlainText "h6"]) `shouldBe` "<h6>h6</h6>"
            it "should render Hgroup" $
                render (SectionTag Hgroup [] [PlainText "hgroup"]) `shouldBe` "<hgroup>hgroup</hgroup>"
            it "should render Header" $
                render (SectionTag Header [] [PlainText "header"]) `shouldBe` "<header>header</header>"
            it "should render Footer" $
                render (SectionTag Footer [] [PlainText "footer"]) `shouldBe` "<footer>footer</footer>"
            it "should render Address" $
                render (SectionTag Address [] [PlainText "address"]) `shouldBe` "<address>address</address>"
        describe "GroupingTag" $ do
            it "should render P" $
                render (GroupingTag P [] $ [PlainText "p"]) `shouldBe` "<p>p</p>"
            it "should render Hr" $
                render (GroupingTag Hr [] $ [PlainText "hr"]) `shouldBe` "<hr>hr</hr>"
            it "should render Blockquote" $
                render (GroupingTag Blockquote [] $ [PlainText "blockquote"]) `shouldBe` "<blockquote>blockquote</blockquote>"
            it "should render Ol" $
                render (GroupingTag Ol [] $ [PlainText "ol"]) `shouldBe` "<ol>ol</ol>"
            it "should render Ul" $
                render (GroupingTag Ul [] $ [PlainText "ul"]) `shouldBe` "<ul>ul</ul>"
            it "should render Menu" $
                render (GroupingTag Menu [] $ [PlainText "menu"]) `shouldBe` "<menu>menu</menu>"
            it "should render Li" $
                render (GroupingTag Li [] $ [PlainText "li"]) `shouldBe` "<li>li</li>"
            it "should render Dl" $
                render (GroupingTag Dl [] $ [PlainText "dl"]) `shouldBe` "<dl>dl</dl>"
            it "should render Dt" $
                render (GroupingTag Dt [] $ [PlainText "dt"]) `shouldBe` "<dt>dt</dt>"
            it "should render Dd" $
                render (GroupingTag Dd [] $ [PlainText "dd"]) `shouldBe` "<dd>dd</dd>"
            it "should render Figure" $
                render (GroupingTag Figure [] $ [PlainText "figure"]) `shouldBe` "<figure>figure</figure>"
            it "should render Figcaption" $
                render (GroupingTag Figcaption [] $ [PlainText "figcaption"]) `shouldBe` "<figcaption>figcaption</figcaption>"
            it "should render Main" $
                render (GroupingTag Main [] $ [PlainText "main"]) `shouldBe` "<main>main</main>"
            it "should render Div" $
                render (GroupingTag Div [] $ [PlainText "div"]) `shouldBe` "<div>div</div>"
        describe "TextTag" $ do
            it "should render Em" $
                render (TextTag Em [] $ [PlainText "em"]) `shouldBe` "<em>em</em>"
            it "should render Strong" $
                render (TextTag Strong [] [PlainText "strong"]) `shouldBe` "<strong>strong</strong>"
            it "should render Small" $
                render (TextTag Small [] [PlainText "small"]) `shouldBe` "<small>small</small>"
            it "should render S" $
                render (TextTag S [] [PlainText "s"]) `shouldBe` "<s>s</s>"
            it "should render Cite" $
                render (TextTag Cite [] [PlainText "cite"]) `shouldBe` "<cite>cite</cite>"
            it "should render Q" $
                render (TextTag Q [] [PlainText "q"]) `shouldBe` "<q>q</q>"
            it "should render Dfn" $
                render (TextTag Dfn [] [PlainText "dfn"]) `shouldBe` "<dfn>dfn</dfn>"
            it "should render Abbr" $
                render (TextTag Abbr [] [PlainText "abbr"]) `shouldBe` "<abbr>abbr</abbr>"
            it "should render Ruby" $
                render (TextTag Ruby [] [PlainText "ruby"]) `shouldBe` "<ruby>ruby</ruby>"
            it "should render Rt" $
                render (TextTag Rt [] [PlainText "rt"]) `shouldBe` "<rt>rt</rt>"
            it "should render Rp" $
                render (TextTag Rp [] [PlainText "rp"]) `shouldBe` "<rp>rp</rp>"
            it "should render Data" $
                render (TextTag Data [] [PlainText "data"]) `shouldBe` "<data>data</data>"
            it "should render Time" $
                render (TextTag Time [] [PlainText "time"]) `shouldBe` "<time>time</time>"
            it "should render Code" $
                render (TextTag Code [] [PlainText "code"]) `shouldBe` "<code>code</code>"
            it "should render Var" $
                render (TextTag Var [] [PlainText "var"]) `shouldBe` "<var>var</var>"
            it "should render Samp" $
                render (TextTag Samp [] [PlainText "samp"]) `shouldBe` "<samp>samp</samp>"
            it "should render Kbd" $
                render (TextTag Kbd [] [PlainText "kbd"]) `shouldBe` "<kbd>kbd</kbd>"
            it "should render Sub" $
                render (TextTag Sub [] [PlainText "sub"]) `shouldBe` "<sub>sub</sub>"
            it "should render Sup" $
                render (TextTag Sup [] [PlainText "sup"]) `shouldBe` "<sup>sup</sup>"
            it "should render I" $
                render (TextTag I [] [PlainText "i"]) `shouldBe` "<i>i</i>"
            it "should render B" $
                render (TextTag B [] [PlainText "b"]) `shouldBe` "<b>b</b>"
            it "should render U" $
                render (TextTag U [] [PlainText "u"]) `shouldBe` "<u>u</u>"
            it "should render Mark" $
                render (TextTag Mark [] [PlainText "mark"]) `shouldBe` "<mark>mark</mark>"
            it "should render Bdi" $
                render (TextTag Bdi [] [PlainText "bdi"]) `shouldBe` "<bdi>bdi</bdi>"
            it "should render Bdo" $
                render (TextTag Bdo [] [PlainText "bdo"]) `shouldBe` "<bdo>bdo</bdo>"
            it "should render Span" $
                render (TextTag Span [] [PlainText "span"]) `shouldBe` "<span>span</span>"
            it "should render Br" $
                render (TextTag Br [] [PlainText "br"]) `shouldBe` "<br>br</br>"
            it "should render Wbr" $
                render (TextTag Wbr [] [PlainText "wbr"]) `shouldBe` "<wbr>wbr</wbr>"
        describe "EditTag" $ do
            it "should render Ins" $
                render (EditTag Ins [] [PlainText "ins"]) `shouldBe` "<ins>ins</ins>"
            it "should render Del" $
                render (EditTag Del [] [PlainText "del"]) `shouldBe` "<del>del</del>"
