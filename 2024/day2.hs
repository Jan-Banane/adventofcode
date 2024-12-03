solveDay2Part1 :: FilePath -> IO Int
solveDay2Part1 = solve part1

solveDay2Part2 :: FilePath -> IO Int
solveDay2Part2 = solve part2

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

prepareInput :: String -> [[Int]]
prepareInput = map (map read  . words) . lines

isIncreasing :: [Int] -> Bool
isIncreasing [x] = True
isIncreasing (x1:x2:xs)
 | x1+1 <= x2 && x1+3 >= x2 = isIncreasing (x2:xs)
 | otherwise = False

--Part 1
part1 :: String -> Int
part1 = length . filter (\list -> isIncreasing (reverse list) || isIncreasing list) . prepareInput

--Part 2
part2 :: String -> Int
part2 = length . filter listMatchesCirteria . prepareInput

listMatchesCirteria :: [Int] -> Bool
listMatchesCirteria list =
    isIncreasingWithInaccuracy (reverse list) ||
    isIncreasingWithInaccuracy list ||
    (isIncreasing . tail . reverse) list ||
    (isIncreasing . tail) list

isIncreasingWithInaccuracy :: [Int] -> Bool
isIncreasingWithInaccuracy [x] = True
isIncreasingWithInaccuracy (x1:x2:xs)
 | x1+1 <= x2 && x1+3 >= x2 = isIncreasingWithInaccuracy (x2:xs)
 | otherwise = isIncreasing (x1:xs)