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

isDecreasing :: [Int] -> Bool
isDecreasing [x] = True
isDecreasing (x1:x2:xs)
 | x1-1 >= x2 && x1-3 <= x2 = isDecreasing (x2:xs)
 | otherwise = False

isIncreasing :: [Int] -> Bool
isIncreasing [x] = True
isIncreasing (x1:x2:xs)
 | x1+1 <= x2 && x1+3 >= x2 = isIncreasing (x2:xs)
 | otherwise = False

--Part 1
part1 :: String -> Int
part1 = length . filter (\list -> isDecreasing list || isIncreasing list) . prepareInput

--Part 2
part2 :: String -> Int
part2 = length . filter listMatchesCirteria . prepareInput

listMatchesCirteria :: [Int] -> Bool
listMatchesCirteria (x:xs) =
    isDecreasingWithInaccuracy (x:xs) || isIncreasingWithInaccuracy (x:xs) || isDecreasing xs || isIncreasing xs

isDecreasingWithInaccuracy :: [Int] -> Bool
isDecreasingWithInaccuracy [x] = True
isDecreasingWithInaccuracy (x1:x2:xs)
 | x1-1 >= x2 && x1-3 <= x2 = isDecreasingWithInaccuracy (x2:xs)
 | otherwise = isDecreasing (x1:xs)

isIncreasingWithInaccuracy :: [Int] -> Bool
isIncreasingWithInaccuracy [x] = True
isIncreasingWithInaccuracy (x1:x2:xs)
 | x1+1 <= x2 && x1+3 >= x2 = isIncreasingWithInaccuracy (x2:xs)
 | otherwise = isIncreasing (x1:xs)