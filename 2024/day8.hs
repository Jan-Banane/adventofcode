{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import qualified Data.Set as S

-- Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

part1 input = S.size $ filterAntinodes (length towerMap, length (head towerMap)) $ S.unions $ map (S.unions . S.toList . S.map (uncurry getAntinodes) . getAllCombinations . getLocations towerMap) allChars
    where towerMap = lines input

part2 input = S.size $ S.fromList $ filterAntinodes' (length towerMap, length (head towerMap)) $ concatMap ( concatMap (uncurry getAntinodes') . S.toList . getAllCombinations . getLocations towerMap) allChars
    where towerMap = lines input

allChars :: [Char]
allChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

getAllCombinations :: S.Set (Int, Int) -> S.Set ((Int,Int),(Int,Int))
getAllCombinations locations = S.fromList $ permutate $ S.toList locations

permutate :: [b] -> [(b, b)]
permutate (x:xs) = map (\a -> (x,a)) xs ++ permutate xs
permutate [] = []

getLocations :: [String] -> Char -> S.Set (Int,Int)
getLocations towerMap char = S.fromList $ concatMap (\(a,list) -> map (\b -> (a,b)) list) (filter (\(_,b) -> b /= []) $ zip [0..] $ map (getLocationsLine char) towerMap)

getLocationsLine :: Char -> String -> [Int]
getLocationsLine char = map fst . filter (\(_,b) -> b == char) . zip [0..]

getAntinodes :: (Int,Int) -> (Int, Int) -> S.Set (Int,Int)
getAntinodes (x1,y1) (x2,y2) = S.fromList [(x2+distanceX,y2+distanceY),(x1-distanceX,y1-distanceY)]
    where distanceX = x2-x1
          distanceY = y2-y1

getAntinodes' :: (Int,Int) -> (Int, Int) -> [(Int,Int)]
getAntinodes' (x1,y1) (x2,y2) = concatMap ((\(distX,distY) -> [(x2+distX,y2+distY),(x1-distX,y1-distY)]) . (\num -> (distanceX * num,distanceY * num))) [0..50]
    where distanceX = x2-x1
          distanceY = y2-y1

filterAntinodes' :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
filterAntinodes' (xRange,yRange) = filter (\(x,y) -> x >= 0 && x < xRange && y >= 0 && y < yRange)

filterAntinodes :: (Int,Int) -> S.Set (Int,Int) -> S.Set (Int,Int)
filterAntinodes (xRange,yRange) = S.filter (\(x,y) -> x >= 0 && x < xRange && y >= 0 && y < yRange)