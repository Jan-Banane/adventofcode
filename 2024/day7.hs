solveDay7Part1 :: FilePath -> IO Int
solveDay7Part1 = solve part1

-- Takes about 01 min 15 sec to execute
solveDay7Part2 :: FilePath -> IO Int
solveDay7Part2 = solve part2

--Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

prepareInput :: String -> [(Int, [Int])]
prepareInput = map ((\(x,_:ys) -> (read x, map read $ words ys)) . break (==':')) . lines

allResultsWithOp :: [Int -> Int -> Int] -> [Int] -> [Int]
allResultsWithOp _ [int] = [int]
allResultsWithOp ops (val:remain) = concatMap ((\func -> map func (allResultsWithOp ops remain)) . (\op -> op val)) ops

--Part 1
part1 :: String -> Int
part1 = sum . map fst . filter isValid . prepareInput

isValid :: (Int, [Int]) -> Bool
isValid (res,values) = res `elem` allResultsWithOp [(+),(*)] (reverse values)

--Part 2
part2 :: String -> Int
part2 = sum . map fst . filter isValidWithConcat . prepareInput

isValidWithConcat :: (Int, [Int]) -> Bool
isValidWithConcat (res,values) = res `elem` allResultsWithOp [(+),(*), flip combine] (reverse values)

combine :: Int -> Int -> Int
combine valA valB = read (show valA ++ show valB)