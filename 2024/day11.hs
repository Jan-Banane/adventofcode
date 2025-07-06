solveDay11Part1 :: FilePath -> IO Int
solveDay11Part1 = solve part1

solveDay11Part2 :: FilePath -> IO Int
solveDay11Part2 = solve part2

--Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

prepareInput :: String -> [Int]
prepareInput = map read  . words

blinks :: Int -> [Int] -> [Int]
blinks 1 map = calcBlink map
blinks count map = blinks (count-1) (calcBlink map)

calcBlink :: [Int] -> [Int]
calcBlink [] = []
calcBlink (x:xs)
 | x == 0 = 1:calcBlink xs
 | even digitLength = (\(q,r) -> q:r:calcBlink xs) $ x `divMod` (10 ^ (digitLength `div` 2))
 | otherwise = (x * 2024):calcBlink xs
 where digitLength = length $ show x

--Part 1
part1 :: String -> Int
part1 input = length $ blinks 25 $ prepareInput input

--Part 2
part2 :: String -> Int
part2 input = length $ blinks 75 $ prepareInput input
