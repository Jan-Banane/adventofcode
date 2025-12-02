import Data.Bifunctor
import qualified Data.Text as T

solveDay2Part1 :: FilePath -> IO Int
solveDay2Part1 = solve part1

solveDay2Part2 :: FilePath -> IO Int
solveDay2Part2 = solve part2

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

parseInputs :: String -> [(Int, Int)]
parseInputs = map (bimap (read . T.unpack) (read . T.unpack . T.tail) . T.break (=='-')) . T.split (==',') . T.pack

createSequence :: Int -> Int -> [Int]
createSequence min max
 | min >= max = [max]
 | otherwise = min:createSequence (min+1) max

getInvalidsSum :: (String -> Bool) -> [Int] -> Int
getInvalidsSum invalidator = sum . filter (invalidator . show)

--Part 1
isTwiceRepeated :: String -> Bool
isTwiceRepeated sequence = uncurry (==) $ splitAt (length sequence `div` 2) sequence

part1 :: String -> Int
part1 = getInvalidsSum isTwiceRepeated . concatMap (uncurry createSequence) . parseInputs

--Part 2
isAnyRepeated :: String -> Bool
isAnyRepeated sequence = any ((\(x:xs) -> all (==x) xs) . sliceList sequence) [1..(length sequence `div` 2)]

sliceList :: String -> Int -> [String]
sliceList [] _ = []
sliceList list size = take size list:sliceList (drop size list) size

part2 :: String -> Int
part2 = getInvalidsSum isAnyRepeated . concatMap (uncurry createSequence) . parseInputs