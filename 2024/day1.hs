import Data.List (transpose, sort)

solvePart1 :: FilePath -> IO Int
solvePart1 = solve part1

solvePart2 :: FilePath -> IO Int
solvePart2 = solve part2 

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

prepareInput :: String -> ([Int], [Int])
prepareInput input = seperate $ transpose $ map (map read  . words) $ lines input

seperate :: [[Int]] -> ([Int], [Int])
seperate [listA, listB] = (listA,listB)
seperate _ = ([],[])

--Part 1
part1 :: String -> Int
part1 = getAllDistance . prepareInput

getAllDistance :: ([Int], [Int]) -> Int
getAllDistance (listA,listB) = sum $ zipWith distance (sort listA) (sort listB)

distance :: Int -> Int -> Int
distance numA numB
 | numA > numB = numA - numB
 | otherwise = numB - numA

--Part 2
part2 :: String -> Int
part2 = uncurry countAndMultiplyAll . prepareInput

countAndMultiplyAll :: [Int] -> [Int] -> Int
countAndMultiplyAll [] _ = 0
countAndMultiplyAll (head:remainder) list = countAndMultiply head list + countAndMultiplyAll remainder list

countAndMultiply :: Int -> [Int] -> Int
countAndMultiply a list = a * length (filter (a ==) list)