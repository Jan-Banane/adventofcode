{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Text as T

-- gets input of a file, then extracts from every line the digit and adds them
-- together. See https://adventofcode.com/2023/day/1 for more info
part1 :: FilePath -> IO Integer
part1 fileName = do
    text <- readFile fileName
    let res = sum $ map getNumber $ lines text
    return res

--gets input of a file, then extracts from every line the digit and adds them
-- together. See https://adventofcode.com/2023/day/1 for more info
part2 :: FilePath -> IO Integer
part2 input = do
    text <- readFile input
    let res = sum $ map getNumberFor2nd $ lines text
    return res

-- does as the func says
replaceWrittenWithNum :: (T.Text -> T.Text) -> String -> String
replaceWrittenWithNum func input = T.unpack $ foldl (\inp (str,int) -> T.replace (func str) (T.concat [func str,int] ) inp) (func $ T.pack input) toReplace
    where toReplace = zip ["one","two","three","four","five","six","seven","eight","nine"] (map (T.pack . show) [1..])

-- gets the first and last digit of a string and converts them to a two digit
-- number e.g. "a1b2c" will result to 12 but first, it replaces written numbers
-- with normal numbers
getNumberFor2nd :: String -> Integer
getNumberFor2nd text = read $ map (\func -> head $ (\inp -> [x | x <- inp , isNumber x]) $ replaceWrittenWithNum func text ) [id,T.reverse]

-- gets the first and last digit of a string and converts them to a two digit
-- number e.g. "a1b2c" will result to 12
getNumber :: String -> Integer
getNumber text = read $ map ($ [x | x <- text , isNumber x]) [head, last]