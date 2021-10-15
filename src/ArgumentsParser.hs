module ArgumentsParser (

    ) where

import Parser
import ClassyPrelude

-- runParseArguments :: Text -> Maybe (Text, Text, Text)
-- runParseArguments 

parseArguments :: TemplateParser (Text, Text, Bool)
parseArguments = (,,)
    <$> parseTextArgument "src"
    <*> parseTextArgument "dest"
    <*> parseBoolArgument 'w' "watch"

parseTextArgument :: Text -> TemplateParser Text
parseTextArgument name = parseText ("--" <> name) *> ws *> spanP (`notElem` predicates)
    where predicates = [' ']

parseBoolArgument :: Element Text -> Text -> TemplateParser Bool
parseBoolArgument short full = True
    <$ (parseText ("--" <> full) <|> parseText ("-" <> pack [short]))
