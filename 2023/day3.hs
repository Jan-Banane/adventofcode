import Data.Char (isDigit)
import Data.List

part1 :: FilePath -> IO Integer
part1 fileName = do
    text <- readFile fileName
    let res = lines text
    return 1

isSymbol :: Char -> Bool
isSymbol c = (not . isDigit) c && (c /= '.')
