import Data.Char (digitToInt)

solveDay3Part1 :: FilePath -> IO Int
solveDay3Part1 = solve part1

solveDay3Part2 :: FilePath -> IO Int
solveDay3Part2 = solve part2

-- Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

findXthDigit :: (Int, Int) -> Int -> [Int] -> Int -> (Int, Int)
findXthDigit (max,pos) _ [] _ = (max,pos)
findXthDigit (max,pos) currPos (x:xs) digitsToIgnore
 | length xs < (digitsToIgnore-1) = (max,pos)
 | x == 9 = (x,currPos)
 | max < x = findXthDigit (x,currPos) (currPos+1) xs digitsToIgnore
 | otherwise = findXthDigit (max,pos) (currPos+1) xs digitsToIgnore

findHighest :: [Int] -> Int -> (Int, [Int])
findHighest line = (\(val,pos) -> (val,drop pos line)) . findXthDigit (-1,0) 1 line 

findJolts :: Int -> [Int] -> Int
findJolts 0 _ = 0
findJolts amount line = val*(10^(amount-1)) + findJolts (amount-1) remaining
 where (val,remaining) = findHighest line amount

--Part 1
part1 :: String -> Int
part1 = sum . map (findJolts 2) . parseInput

--Part 2
part2 :: String -> Int
part2  = sum . map (findJolts 12) . parseInput