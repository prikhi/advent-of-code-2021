module ParseHelper where

import Control.Monad (void)
import Text.ParserCombinators.ReadP


parseInput :: ReadP x -> String -> [x]
parseInput parser =
    fst . head . filter ((== "") . snd) . readP_to_S (sepBy parser newline <* end)
    where
        newline =
            choice
                [ void $ char '\r' *> char '\n'
                , void $ char '\n'
                ]
        end =
            many $ choice [newline, eof]
