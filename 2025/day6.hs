--Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

swapRowAndColumn :: [[a]] -> [[a]]
swapRowAndColumn [] = []
swapRowAndColumn twoDList = swapped:swapRowAndColumn remainder
 where swapped = getColumn twoDList
       remainder = removeColumn twoDList

removeColumn :: [[a]] -> [[a]]
removeColumn [] = []
removeColumn ([]:remaining) = []
removeColumn ((x:xs):remaining) = xs:removeColumn remaining

getColumn :: [[a]] -> [a]
getColumn [] = []
getColumn ([]:remaining) = []
getColumn ((x:xs):remaining) = x:getColumn remaining

--Part 1
parseInputPart1 :: String -> [[String]]
parseInputPart1 = map reverse . swapRowAndColumn . map words . lines

solveColumn :: [String] -> Int
solveColumn ("+":xs) = sum $ map read xs
solveColumn ("*":xs) = product $ map read xs
solveColumn _ = 0

part1 :: String -> Int
part1 = sum . map solveColumn . parseInputPart1

--Part 2
parseInputPart2 :: String -> [String]
parseInputPart2 = map removeWhiteSpace . swapRowAndColumn . lines

removeWhiteSpace :: String -> String
removeWhiteSpace [] = []
removeWhiteSpace (x:xs)
 | x == ' ' = removeWhiteSpace xs
 | otherwise = x:removeWhiteSpace xs

solveNextCalc :: [String] -> Int
solveNextCalc [] = 0
solveNextCalc (x:xs)
 | last x == '*' = (\(toProd, _:remaining) -> (product (map read toProd) * read (init x)) + solveNextCalc remaining ) $ break (=="") xs
 | last x == '+' = (\(toSum, _:remaining) -> (sum (map read toSum) + read (init x)) + solveNextCalc remaining ) $ break (=="") xs

part2 :: String -> Int
part2 = solveNextCalc . parseInputPart2