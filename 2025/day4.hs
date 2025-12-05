import qualified Data.Set as S

solveDay4Part1 :: FilePath -> IO Int
solveDay4Part1 = solve part1

solveDay4Part2 :: FilePath -> IO Int
solveDay4Part2 = solve part2

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

allRollPos :: [String] -> S.Set (Int, Int)
allRollPos text = S.fromList $ filter ((Just '@' ==) . elementAt text) $ indexesToAllElem (length $ head text) (length text)

indexesToAllElem :: Int -> Int -> [(Int, Int)]
indexesToAllElem lenX lenY = concatMap (\y -> map (,y) [0..lenX-1]) [0..lenY-1]

elementAt :: [String]-> (Int, Int) -> Maybe Char
elementAt text (x,y)
 | 0 <= x && x < length (head text) && 0 <= y && y < length text = Just (text !! y !! x)
 | otherwise = Nothing

removeRmblRolls :: S.Set (Int, Int) -> S.Set (Int, Int)
removeRmblRolls rollPoss = S.difference rollPoss $ S.filter (canRollBeRemoved rollPoss) rollPoss

canRollBeRemoved :: S.Set (Int, Int) -> (Int, Int) -> Bool
canRollBeRemoved rollPoss (x,y) = 4 > length (filter (\(a,b) -> S.member (x+a,y+b) rollPoss) [(1,0),(-1,0),(0,1),(0,-1),(1,1),(-1,1),(1,-1),(-1,-1)])

--Part 1
part1 :: String -> Int
part1 input = S.size rollPoss - S.size (removeRmblRolls rollPoss)
 where rollPoss = allRollPos $ lines input

--Part 2
removeAllRmblRolls :: S.Set (Int, Int) -> S.Set (Int, Int)
removeAllRmblRolls rollPoss
 | S.size rollPoss == S.size updatedRollPoss = rollPoss
 | otherwise = removeAllRmblRolls updatedRollPoss
 where updatedRollPoss = removeRmblRolls rollPoss

part2 :: String-> Int
part2 input = S.size rollPoss - S.size (removeAllRmblRolls rollPoss)
 where rollPoss = allRollPos $ lines input