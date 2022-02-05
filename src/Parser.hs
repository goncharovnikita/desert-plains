-- {-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Parser ( 
    parseTemplate,
    parseChar,
    parseCharCond,
    parseText,
    parseTitle,
    parseHead,
    execTemplateParser,
    execTemplateParserForResult,
    parseStyleProperty,
    parseAttributes,
    parseBasicAttribute,
    parseInterpolatedAttribute,
    parseBoolAttribute,
    parseInterpolatedString,
    parseClass,
    parseAllClasses,
    parseTextWithPrefix,
    parseStyleContents,
    parseStyleNode,
    parseComponentLogic,
    parseComponentProps,
    parseBodyContentItem,
    parseBodyContents,
    parseComponent,
    parseComponents,
    spanP,
    ws,
    interWhitespace,
    interpolatedStringLiteral,
    TemplateParser(..)
) where

import ClassyPrelude
import Model
import Data.Char (isSpace, isUpper, isLetter)
import Control.Monad.Writer
import Helpers

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
parseTemplate = Html
    <$> parseHead
    <*> parseBody
    <*> parseComponents


----- HEAD PARSERS
parseHead :: TemplateParser HeadNodeFamily
parseHead = HeadNode <$> parseBaseTag "head" parseHeadContents

parseHeadContents :: TemplateParser [HeadNodeFamily]
parseHeadContents = many (parseTitle <|> parseHeadStyle)

parseTitle :: TemplateParser HeadNodeFamily
parseTitle = Title <$> parseBaseTag "title" stringLiteral

parseHeadStyle :: TemplateParser HeadNodeFamily
parseHeadStyle = StyleNode <$> parseStyleNode

----- BODY PARSERS
parseBody :: TemplateParser BodyNodeFamily
parseBody = uncurry BodyNode <$> parseTagWithAttributes "body" parseBodyContents

parseBodyContents :: TemplateParser [BodyNodeFamily]
parseBodyContents = many parseBodyContentItem

parseBodyContentItem :: TemplateParser BodyNodeFamily
parseBodyContentItem = 
    interWhitespace $ TextNode <$> stringLiteral
    <|> bodyParser H1 "h1"
    <|> bodyParser Div "div"
    <|> bodyParser A "a"
    <|> bodyParser Button "button"
    <|> interWhitespace parseComponentExpression
    <|> BodyInterpolatedString <$> interWhitespace parseInterpolatedString
   

bodyParser :: ([Attribute] -> [BodyNodeFamily] -> BodyNodeFamily) -> Text -> TemplateParser BodyNodeFamily
bodyParser constr tagName = interWhitespace $ uncurry constr <$> parseTagWithAttributes tagName parseBodyContents

parseComponentExpression :: TemplateParser BodyNodeFamily
parseComponentExpression = (\fNameLetter restName attrs children -> ComponentExpression (pack [fNameLetter] <> restName) attrs children)
    <$> (parseChar '[' *> parseCharCond isUpper)
    <*> contiguousTextLiteral
    <*> interWhitespace parseAttributes
    <*> (parseBodyContents <* parseChar ']')

parseInterpolatedString :: TemplateParser InterpolatedString
parseInterpolatedString = InterpolatedString
    <$> (parseChar '{' *> some interpolatedStringLiteral <* parseChar '}')

interpolatedStringLiteral :: TemplateParser InterpolatedString
interpolatedStringLiteral =
            (InterpolatedValue <$> (parseChar '#' *> spanP (`notElem` [' ', '}', '#'])))
        <|> (InterpolatedText <$> strictSpanP (`notElem` ['#', '}']))

----- COMPONENTS PARSERS

parseComponents :: TemplateParser [Component]
parseComponents = many (interWhitespace parseComponent)

parseComponent :: TemplateParser Component
parseComponent = Component
    <$> parseComponentName
    <*> (interWhitespace parseComponentProps <|> (ws >> return []))
    <*> interWhitespace parseComponentLogic
    <*> (some (interWhitespace parseBodyContentItem) <* parseChar ']')

parseComponentName :: TemplateParser Text
parseComponentName = (\fLetterOfName restName -> pack [fLetterOfName] <> restName)
    <$> (parseChar '[' *> parseCharCond isUpper)
    <*> spanP isLetter

parseComponentProps :: TemplateParser [Text]
parseComponentProps = parseChar '[' *> propsLiteral <* parseChar ']'
    where propsLiteral = sepBy (parseChar ',' *> ws) (spanP isLetter)

parseComponentLogic :: TemplateParser [Text]
parseComponentLogic = many (interWhitespace parseComponentLogicItem)
    where parseComponentLogicItem = parseText "{{" *> wsOrNewline *> logicString <* wsOrNewline <* parseText "}}"
          logicString = longSpanP 2 (/="}}")

----- STYLE PARSERS
parseStyleNode :: TemplateParser StyleFamily
parseStyleNode = Style <$> parseBaseTag "style" parseStyleContents

parseStyleContents :: TemplateParser [StyleFamily]
parseStyleContents = many parseStyleElement 

parseStyleElement :: TemplateParser StyleFamily
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

parseStyleProperty :: TemplateParser StyleFamily
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
parseBaseTag tag childParser = interWhitespace (parseText ("[" <> tag) *> interWhitespace childParser <* parseChar ']')

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
    <|> (ws *> parseBoolAttribute <* ws)
    <|> (ws *> parseInterpolatedAttribute <* ws)
    )

parseAllClasses :: TemplateParser Attribute
parseAllClasses = ClassList <$> some parseClass

parseClass :: TemplateParser Text
parseClass = parseChar '.' *> contiguousTextLiteral

parseBasicAttribute :: TemplateParser Attribute
parseBasicAttribute = curry BasicAttribute
    <$> (contiguousTextLiteral <* parseChar '=')
    <*> stringLiteral

parseBoolAttribute :: TemplateParser Attribute
parseBoolAttribute = BoolAttribute <$> (strictSpanP isLetter <* strictSpanP (`elem` [' ', '\n', ']']))

parseInterpolatedAttribute :: TemplateParser Attribute
parseInterpolatedAttribute = curry InterpolatedAttribute
    <$> (contiguousTextLiteral <* parseText "={")
    <*> (interpolatedAttributeValue <* parseChar '}')
        where interpolatedAttributeValue = spanP (`notElem` predicates)
              predicates = ['}', '\n']

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
