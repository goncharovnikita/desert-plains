module Lexer where
import ClassyPrelude
import Parser.Model (Block)

runLexer :: Text -> Either Text [Block]
runLexer t = 
