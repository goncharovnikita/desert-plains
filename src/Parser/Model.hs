module Parser.Model (
    Block(..)
) where
import ClassyPrelude


data Block = Block Text [Block]
    deriving (Show, Eq, Ord)
