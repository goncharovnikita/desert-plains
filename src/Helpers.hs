module Helpers where 

import ClassyPrelude

longSpan :: Int -> (Text -> Bool) -> Text -> (Text, Text)
longSpan predicateLength predicate txt
    | length txt < predicateLength = (txt, "")
    | otherwise =
        let textToCheck = take predicateLength txt
            nonNullTxt = impureNonNull txt
        in if predicate textToCheck then
            let (xs', ys') = longSpan predicateLength predicate (tail nonNullTxt)
            in (pack [head nonNullTxt] <> xs', ys')
        else
            ("", txt)
