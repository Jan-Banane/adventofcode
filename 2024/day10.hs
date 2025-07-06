import Data.Char (digitToInt)
import qualified Data.Set as S
import Data.Bifunctor

solveDay10Part1 :: FilePath -> IO Int
solveDay10Part1 = solve part1

solveDay10Part2 :: FilePath -> IO Int
solveDay10Part2 = solve part2

--Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

pathPattern :: [(Int, Int)]
pathPattern = [(0,-1),(1,0),(0,1),(-1,0)]

indexesToAllElem :: Int -> Int -> [(Int, Int)]
indexesToAllElem lenX lenY = concatMap (\y -> map (,y) [0..lenX-1]) [0..lenY-1]

heightAt :: [[Int]]-> (Int, Int) -> Int
heightAt topomap (x,y)
 | 0 <= x && x < length (head topomap) && 0 <= y && y < length topomap = topomap !! y !! x
 | otherwise = -1

--Part 1
part1 :: String -> Int
part1 input = sum $ map (S.size . findAllEnds topoMap 0) $ filter ((0 ==) . heightAt topoMap) allIndexes
 where
    topoMap = map (map digitToInt) $ lines input
    allIndexes = indexesToAllElem (length $ head topoMap) (length topoMap)

findAllEnds :: [[Int]] -> Int -> (Int, Int) -> S.Set (Int,Int)
findAllEnds _ 9 zipped = S.singleton zipped
findAllEnds topoMap height (x,y) = S.unions $ map (findAllEnds topoMap (height+1)) $ filter (((height+1) ==) . heightAt topoMap) nextLocations
 where nextLocations = map (bimap (x+) (y+)) pathPattern

--Part 2
part2 :: String -> Int
part2 input = sum $ map (getRating topoMap 0) $ filter ((0 ==) . heightAt topoMap) allIndexes
 where
    topoMap = map (map digitToInt) $ lines input
    allIndexes = indexesToAllElem (length $ head topoMap) (length topoMap)

getRating :: [[Int]] -> Int -> (Int, Int) -> Int
getRating _ 9 zipped = 1
getRating topoMap height (x,y) = sum $ map (getRating topoMap (height+1)) $ filter (((height+1) ==) . heightAt topoMap) nextLocations
 where nextLocations =map (bimap (x+) (y+)) pathPattern