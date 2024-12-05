{-# LANGUAGE OverloadedStrings     #-}

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

solveDay5Part1 :: FilePath -> IO Int
solveDay5Part1 = solve part1

solveDay5Part2 :: FilePath -> IO Int
solveDay5Part2 = solve part2 

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

readRules :: [Char] -> M.Map Int [Int]
readRules = M.fromListWith (++) . map ((\(x,y) -> (read x, [read y])) . splits '|') . fst . splits "" . lines

readUpdatePages :: String -> [[Int]]
readUpdatePages = map (map (read . T.unpack) . T.splitOn "," . T.pack) . snd . splits "" . lines

splits :: Eq p => p -> [p] -> ([p], [p])
splits splitter list = (x,y)
    where (x,_:y) = break (==splitter) list

isValid :: M.Map Int [Int] -> [Int] -> [Int] -> Bool
isValid _ _ [] = True
isValid rules invalids (x:xs) = x `notElem` invalids && isValid rules (addition++invalids) xs
    where addition = fromMaybe [] (M.lookup x rules)

centerElement :: [Int] -> Int
centerElement list = list !! (length list `div` 2)

--Part 1
part1 :: String -> Int
part1 input = sum $ map centerElement $ filter (isValid (readRules input) [] . reverse) $ readUpdatePages input

--Part 2
part2 :: String -> Int
part2 input = sum $ map (centerElement . sort rules) $ filter (not . isValid rules []) $ map reverse $ readUpdatePages input
 where rules = readRules input

sort :: M.Map Int [Int] -> [Int] -> [Int]
sort rules list
 | list == listOnceSorted = list
 | otherwise = sort rules listOnceSorted
 where listOnceSorted = sortOnce rules [] list

sortOnce :: M.Map Int [Int] -> [Int] -> [Int] -> [Int]
sortOnce _ _ [x] = [x]
sortOnce rules prevForbidden (x1:x2:xs)
 | x2 `elem` forbiddenX1 = x2:sortOnce rules forbiddenX2 (x1:xs)
 | otherwise =  x1:sortOnce rules forbiddenX1 (x2:xs)
 where
    forbiddenX1 = fromMaybe [] (M.lookup x1 rules)++prevForbidden
    forbiddenX2 = fromMaybe [] (M.lookup x2 rules)++prevForbidden