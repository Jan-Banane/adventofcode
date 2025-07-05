{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import qualified Data.Set as S

solveDay8Part1 :: FilePath -> IO Int
solveDay8Part1 = solve part1

solveDay8Part2 :: FilePath -> IO Int
solveDay8Part2 = solve part2

--Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

allRelevantChars :: [Char]
allRelevantChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

getAllCombinations :: S.Set (Int, Int) -> S.Set ((Int,Int),(Int,Int))
getAllCombinations = S.fromList . permutations . S.toList

permutations :: [b] -> [(b, b)]
permutations (x:xs) = map (\a -> (x,a)) xs ++ permutations xs
permutations [] = []

coordinatesOfChar :: [String] -> Char -> S.Set (Int,Int)
coordinatesOfChar towerMap char = S.fromList $ concatMap (\(a,list) -> map (\b -> (a,b)) list) (filter (\(_,b) -> b /= []) $ zip [0..] $ map (indexOfChar char) towerMap)

indexOfChar :: Char -> String -> [Int]
indexOfChar char = map fst . filter (\(_,b) -> b == char) . zip [0..]

filterAntinodes :: Int -> Int -> S.Set (Int,Int) -> S.Set (Int,Int)
filterAntinodes xRange yRange = S.filter (\(x,y) -> x >= 0 && x < xRange && y >= 0 && y < yRange)

--Part 1
part1 :: String -> Int
part1 input = S.size $ antinodeFilter $ S.unions $ map (S.unions . S.map getAdjacentAntinodes . getAllCombinations . coordinatesOfChar towerMap) allRelevantChars
 where 
    towerMap = lines input
    antinodeFilter = filterAntinodes (length towerMap) (length $ head towerMap)

getAdjacentAntinodes :: ((Int,Int),(Int, Int)) -> S.Set (Int,Int)
getAdjacentAntinodes ((x1,y1),(x2,y2)) = S.fromList [(x2+distanceX,y2+distanceY),(x1-distanceX,y1-distanceY)]
 where
    distanceX = x2-x1
    distanceY = y2-y1

--Part 2
part2 :: String -> Int
part2 input = S.size $ antinodeFilter $ S.unions $ map (S.unions . S.map allAntinodesFinder . getAllCombinations . coordinatesOfChar towerMap) allRelevantChars
 where
    towerMap = lines input
    antinodeFilter = filterAntinodes (length towerMap) (length $ head towerMap)
    allAntinodesFinder = getAllAntinodes (length towerMap) (length (head towerMap))

getAllAntinodes :: Int -> Int -> ((Int,Int),(Int, Int)) -> S.Set (Int,Int)
getAllAntinodes xRange yRange ((x1,y1),(x2,y2)) = S.fromList $ concatMap ((\(distX,distY) -> [(x2+distX,y2+distY),(x1-distX,y1-distY)]) . (\num -> (distanceX * num,distanceY * num))) [0..(max (div xRange distanceX) (div yRange distanceY))]
 where
    distanceX = x2-x1
    distanceY = y2-y1