solveDay1Part1 :: FilePath -> IO Int
solveDay1Part1 = solve part1

solveDay1Part2 :: FilePath -> IO Int
solveDay1Part2 = solve part2

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

parseInputs :: String -> [Int]
parseInputs = map parseRotation . lines

parseRotation :: String -> Int
parseRotation (x:xs)
 | x == 'L' = negate (read xs)
 | x == 'R' = read xs
 | otherwise = error "Bad Input"

--Part 1
countStepsOnZero :: Int-> [Int] -> Int
countStepsOnZero _ [] = 0
countStepsOnZero currval (x:xs)
 | calcPos == 0 = 1 + countStepsOnZero calcPos xs
 | otherwise = countStepsOnZero calcPos xs
 where calcPos = (currval + x) `mod` 100

part1 :: String -> Int
part1 = countStepsOnZero 50 . parseInputs

--Part 2
countStepsThroughZero :: Int -> [Int] -> Int
countStepsThroughZero _ [] = 0
countStepsThroughZero currval (x:xs)
 | x < 0 = countStepsThroughZero (calcPos (-)) (x+1:xs) + if calcPos (-) == 0 then 1 else 0
 | x > 0 = countStepsThroughZero (calcPos (+)) (x-1:xs) + if calcPos (+) == 0 then 1 else 0
 | otherwise = countStepsThroughZero currval xs
 where calcPos direction = (currval `direction` 1) `mod` 100

part2 :: String -> Int
part2 = countStepsThroughZero 50 . parseInputs