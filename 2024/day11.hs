{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import qualified Data.Map as M

solveDay11Part1 :: FilePath -> IO Int
solveDay11Part1 = solve part1

solveDay11Part2 :: FilePath -> IO Int
solveDay11Part2 = solve part2

--Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

prepareInput :: String -> M.Map Int Int
prepareInput = M.fromList . map ((\val -> (val,1)) . read) . words

blinks :: Int -> M.Map Int Int -> M.Map Int Int
blinks 1 = calcBlink
blinks count = blinks (count-1) . calcBlink

calcBlink :: M.Map Int Int -> M.Map Int Int
calcBlink = M.fromListWith (+) . concatMap (\(val,sum) -> map (\uptVal -> (uptVal,sum)) $ blinkVal val) . M.toList

blinkVal :: Int -> [Int]
blinkVal stone
 | stone == 0 = [1]
 | even digitLength = (\(q,r) -> [q, r]) $ stone `divMod` (10 ^ (digitLength `div` 2))
 | otherwise = [stone * 2024]
 where digitLength = length $ show stone

--Part 1
part1 :: String -> Int
part1 input = M.foldl (+) 0 $ blinks 25 $ prepareInput input

--Part 2
part2 :: String -> Int
part2 input = M.foldl (+) 0 $ blinks 75 $ prepareInput input
