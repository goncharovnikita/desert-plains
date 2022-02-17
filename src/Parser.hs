-- {-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Parser where

import ClassyPrelude
import Model
import Data.Char (isSpace, isUpper, isLetter)
import Control.Monad.Writer
import Helpers
import Html.Head (Head(Head))
import Html.Body (Body(Body))
import Html.Attribute
import Html.Style (Style (StyleElement, StyleProperty))
import Component
import Html.BodyTags
import Html.HeadTags (HeadTag (Title, Style, Meta, Base, Link))
import Html.Html (Html(Html))

newtype TemplateParser v = TemplateParser {
    runTemplateParser :: Text -> Writer [Text] (Maybe (Text, v))
    }

execTemplateParser :: TemplateParser a -> Text -> Either Text (a, Text)
execTemplateParser tparser text =
    let (parseResult, logged) = runWriter (runTemplateParser tparser text)
    in case parseResult of
        Nothing -> Left $ unlines logged
        Just (_, a) -> Right (a, unlines logged)

execTemplateParserForResult :: TemplateParser a -> Text -> Either Text a
execTemplateParserForResult tparser text =
    let result = execTemplateParser tparser text
    in case result of
        Left e -> Left e
        Right (x, _) -> Right x

runParser :: TemplateParser a -> Text -> (Maybe (Text, a), [Text])
runParser parser txt = runWriter $ runTemplateParser parser txt

instance Functor TemplateParser where
    f `fmap` a = TemplateParser $ \text ->
        let (parseResult, logged) = runWriter $ runTemplateParser a text
        in case parseResult of
            Nothing -> writer (Nothing, logged)
            Just (text', template) -> writer (Just (text', f template), logged)

instance Applicative TemplateParser where
    f <*> a = TemplateParser $ \text ->
        let (parseResult, logged) = runParser f text
        in case parseResult of
            Nothing -> writer (Nothing, logged)
            Just (text', templateF) ->
                let (parseResult', logged') = runWriter $ runTemplateParser a text'
                in case parseResult' of
                    Nothing -> writer (Nothing, logged')
                    Just (text'', template) -> writer (Just (text'', templateF template), logged')

    pure a = TemplateParser $ \text -> writer (Just (text, a), [])

instance Monad TemplateParser where
    return = pure

    a >>= f = TemplateParser $ \text ->
        let (parseResult, logged) = runWriter $ runTemplateParser a text
        in case parseResult of
            Nothing -> writer (Nothing, logged)
            Just (text', template) ->
                let (parseResult', logged') = runWriter $ runTemplateParser (f template) text'
                in case parseResult' of
                    Nothing -> writer (Nothing, logged')
                    Just (text'', template') -> writer (Just (text'', template'), logged')

instance Alternative TemplateParser where
    empty = TemplateParser $ \_ -> writer (Nothing, [])

    (TemplateParser a) <|> (TemplateParser b) = TemplateParser $ \text ->
        let (parseResult, logged) = runWriter $ a text
        in case parseResult of
            Nothing ->
                let (parseResult', logged') = runWriter $ b text
                in case parseResult' of
                    Nothing -> writer (Nothing, logged')
                    Just res -> writer (Just res, logged')
            Just res -> writer (Just res, logged)


parseTemplate :: TemplateParser Template
parseTemplate = Template <$> (Html <$> parseHead <*> parseBody)


----- HEAD PARSERS
parseHead :: TemplateParser Head
parseHead = uncurry Head <$> parseTagWithAttributes "head" parseHeadContents

parseHeadContents :: TemplateParser [HeadTag]
parseHeadContents = many
    (parseTitle
    <|> parseBase
    <|> parseMeta
    <|> parseLink
    <|> parseHeadStyle
    )

parseTitle :: TemplateParser HeadTag
parseTitle = Title <$> parseBaseTag "title" stringLiteral

parseBase :: TemplateParser HeadTag
parseBase = Base <$> parseSelfClosingTag "base"

parseLink :: TemplateParser HeadTag
parseLink = Link <$> parseSelfClosingTag "link"

parseMeta :: TemplateParser HeadTag
parseMeta = Meta <$> parseSelfClosingTag "meta"

parseHeadStyle :: TemplateParser HeadTag
parseHeadStyle = Style <$> parseBaseTag "style" parseStyleContents

----- BODY PARSERS
parseBody :: TemplateParser Body
parseBody = uncurry Body <$> parseTagWithAttributes "body" parseBodyContents

parseBodyContents :: TemplateParser [BodyTag]
parseBodyContents = many parseBodyContentItem

parseBodyContentItem :: TemplateParser BodyTag
parseBodyContentItem = 
    interWhitespace $
    PlainText <$> stringLiteral
    <|> bodyParser (SectionTag Article) "article"
    <|> bodyParser (SectionTag Section) "section"
    <|> bodyParser (SectionTag Nav) "nav"
    <|> bodyParser (SectionTag Aside) "aside"
    <|> bodyParser (SectionTag H1) "h1"
    <|> bodyParser (SectionTag H2) "h2"
    <|> bodyParser (SectionTag H3) "h3"
    <|> bodyParser (SectionTag H4) "h4"
    <|> bodyParser (SectionTag H5) "h5"
    <|> bodyParser (SectionTag H6) "h6"
    <|> bodyParser (SectionTag Hgroup) "hgroup"
    <|> bodyParser (SectionTag Header) "header"
    <|> bodyParser (SectionTag Footer) "footer"
    <|> bodyParser (SectionTag Address) "address"
   

bodyParser :: ([Attribute] -> [BodyTag] -> BodyTag) -> Text -> TemplateParser BodyTag
bodyParser constr tagName = interWhitespace $ uncurry constr <$> parseTagWithAttributes tagName parseBodyContents

----- STYLE PARSERS
parseStyleContents :: TemplateParser [Style]
parseStyleContents = many parseStyleElement 

parseStyleElement :: TemplateParser Style
parseStyleElement =
    let nodeNameTextLiteral = spanP (`notElem` ['\n', ']', '.', '#', ' '])
        nodeNameLiteral
            =   parseTextWithPrefix "." nodeNameTextLiteral
            <|> parseTextWithPrefix "#" nodeNameTextLiteral
            <|> nodeNameTextLiteral
        nodeNameParser = parseChar '[' *> nodeNameLiteral <* wsOrNewline
    in nodeNameParser >>= \elementName ->
       (many parseStyleProperty <* parseChar ']' <* wsOrNewline) >>= \styleProperties ->
           return $ StyleElement elementName styleProperties 

parseTextWithPrefix :: Text -> TemplateParser Text -> TemplateParser Text
parseTextWithPrefix prefix parser = (<>) <$> parseText prefix <*> parser

parseStyleProperty :: TemplateParser Style
parseStyleProperty = curry StyleProperty
    <$> (ws *> textLiteral <* parseChar ':' <* ws)
    <*> (ws *> propertyLiteral <* parseChar ';' <* wsOrNewline)
    where propertyLiteral = spanP (`notElem` predicates)
          predicates = [';']

----- HELPERS
parseCharCond :: (Element Text -> Bool) -> TemplateParser (Element Text)
parseCharCond cond = TemplateParser $ \text ->
    let text' = fromNullable text
    in case text' of
        Nothing -> writer (Nothing, ["Error parse charCond: empty input provided"])
        Just txt -> case cond $ head txt of
            False -> writer (Nothing, ["Error parse charCond"])
            True -> writer (Just (tail txt, head txt), [])

parseChar :: Element Text -> TemplateParser (Element Text)
parseChar c = TemplateParser $ \text ->
    let text' = fromNullable text
    in case text' of
        Nothing -> writer (Nothing, ["Error parse char: empty input provided"])
        Just txt -> case head txt == c of
            False -> writer (Nothing, [concat ["Error parse char: expected '", pack [c], "', but got '", pack [head txt], "'"]])
            True -> writer (Just (tail txt, head txt), [])

parseText :: Text -> TemplateParser Text
parseText text = TemplateParser $ \text' ->
    let listOfParsers = map parseChar (otoList text)
        seqparser = sequenceA listOfParsers
        (parseResult, logged) = runWriter $ runTemplateParser seqparser text'

    in case parseResult of
        Nothing -> writer (Nothing, [concat ["Error parse text: could not match input '", text, "'"]] <> logged)
        Just (text'', template) -> writer (Just (text'', fromList template), logged)

parseBaseTag :: Text -> TemplateParser a -> TemplateParser a
parseBaseTag tag childParser = interWhitespace
    (parseText ("[" <> tag) *> interWhitespace childParser <* parseChar ']')

parseSelfClosingTag :: Text -> TemplateParser [Attribute]
parseSelfClosingTag tag = interWhitespace
    (parseText ("[" <> tag) *> interWhitespace parseAttributes <* parseChar ']')

parseTagWithAttributes :: Text -> TemplateParser a -> TemplateParser ([Attribute], a)
parseTagWithAttributes tag childParser = TemplateParser $ \text ->
    let contentParser = interWhitespace childParser <* parseChar ']'
        parser = interWhitespace (parseText ("[" <> tag) *> interWhitespace parseAttributes)
        (parseResult, logged) = runParser parser text
    in case parseResult of
        Nothing -> writer (Nothing, logged)
        Just (text', attributes) ->
            let (parseResult', logged') = runParser contentParser text'
            in case parseResult' of
                Nothing -> writer (Nothing, logged' <> ["Could not parse tag contents: ", text'])
                Just (text'', children) -> writer (Just (text'', (attributes, children)), logged')

parseAttributes :: TemplateParser [Attribute]
parseAttributes = many
    (   parseAllClasses
    <|> (ws *> parseBasicAttribute <* ws)
    )

parseAllClasses :: TemplateParser Attribute
parseAllClasses = ClassList <$> some parseClass

parseClass :: TemplateParser Text
parseClass = parseChar '.' *> contiguousTextLiteral

parseBasicAttribute :: TemplateParser Attribute
parseBasicAttribute = curry BasicAttribute
    <$> (contiguousTextLiteral <* parseChar '=')
    <*> stringLiteral

stringLiteral :: TemplateParser Text
stringLiteral = parseChar '"' *> spanP (/= '"') <* parseChar '"'

contiguousTextLiteral :: TemplateParser Text
contiguousTextLiteral = spanP (`notElem` predicates)
    where predicates = [':', '"', '=', ';', ' ', '.', '\n', ']']

textLiteral :: TemplateParser Text
textLiteral = spanP (`notElem` predicates)
    where predicates = [':', ';', '.', '\n']

sepBy :: TemplateParser a -> TemplateParser b -> TemplateParser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

spanP :: (Element Text -> Bool) -> TemplateParser Text
spanP f = TemplateParser $ \text ->
    let (token, xs) = span f text
     in writer (Just (xs, token), [])

strictSpanP :: (Element Text -> Bool) -> TemplateParser Text
strictSpanP f = TemplateParser $ \text ->
    let (token, xs) = span f text
     in if null token then
            writer (Nothing, ["spanP ended up with empty value: " <> xs])
        else
            writer (Just (xs, token), [])


longSpanP :: Int -> (Text -> Bool) -> TemplateParser Text
longSpanP i f = TemplateParser $ \text -> do
    let (token, xs) = longSpan i f text
    tell ["LongSpan: token: ", token, "xs: ", xs]
    writer (Just (xs, token), [])

ws :: TemplateParser Text
ws = spanP isSpace

interWhitespace :: TemplateParser a -> TemplateParser a
interWhitespace parser = wsOrNewline *> parser <* wsOrNewline

wsOrNewline :: TemplateParser Text
wsOrNewline = spanP (\c -> isSpace c || (c == '\n'))
