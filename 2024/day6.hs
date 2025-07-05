{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
import Data.Maybe

solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

addCross :: [[Char]] -> (Int, Int) -> [[Char]]
addCross list (x,y) = (\(a,b1:b2) -> a++(addCrossLine b1 x:b2)) $ splitAt y list

addCrossLine :: [Char] -> Int -> [Char]
addCrossLine list x = (\(a,_:b) -> a++('X':b)) $ splitAt x list

elementAt :: [String]-> (Int, Int) -> Maybe Char
elementAt text (x,y)
 | 0 <= x && x < length (head text) && 0 <= y && y < length text = Just (text !! y !! x)
 | otherwise = Nothing

findGuard :: String -> (Int, Int)
findGuard input = (x,y)
    where (before,_) = break (=='^') input
          y = length (lines before) -1
          x = length $ last $ lines before

part1 :: String -> Int
part1 input = length $ concatMap (filter (=='X')) $ move 0 (lines input) $ findGuard input

movePattern :: [(Int, Int)]
movePattern = [(0,-1),(1,0),(0,1),(-1,0)]

obstacle :: [Char]
obstacle = ['U','R','D','L']

getObstacle :: Int -> Char
getObstacle no = obstacle !! (no `mod` 4)

getPattern :: Int -> (Int, Int)
getPattern no = movePattern !! (no `mod` 4)

move patternNo list (x,y)
 | isNothing isObstacle = updatedList
 | Just True == isObstacle = move (patternNo+1) list (x,y)
 | otherwise = move patternNo (addCross list (x,y)) ((\(a,b) -> (x+a,y+b)) (getPattern patternNo))
 where isObstacle = (=='#') <$> elementAt list ((\(a,b) -> (x+a,y+b)) (getPattern patternNo))
       updatedList = addCross list (x,y)

part2 input = length $ filter id $ move' 0 (lines input) $ findGuard input

move' :: Int -> [[Char]] -> (Int, Int) -> [Bool]
move' patternNo list (x,y)
 | isNothing isObstacle = []
 | Just '#' == isObstacle = move' (patternNo+1) list (x,y)
 | Just 'X' == isObstacle = move' patternNo (addCross list (x,y)) ((\(a,b) -> (x+a,y+b)) (getPattern patternNo))
 | otherwise =
    createsLoop (getObstacle patternNo) (patternNo+1) (addChar (getObstacle patternNo) list ((\(a,b) -> (x+a,y+b)) (getPattern patternNo))) (x,y) :move' patternNo (addCross list (x,y)) ((\(a,b) -> (x+a,y+b)) (getPattern patternNo))
 where isObstacle = elementAt list ((\(a,b) -> (x+a,y+b)) (getPattern patternNo))
       updatedList = addCross list (x,y)

createsLoop :: Char -> Int -> [[Char]] -> (Int, Int) -> Bool
createsLoop obs patternNo list (x,y)
 | char == 'Q' = False
 | obs == char = (getObstacle patternNo == obs) || createsLoop obs (patternNo+1) list (x,y)
 | char `elem` '#':obstacle = createsLoop obs (patternNo+1) list (x,y)
 | otherwise = createsLoop obs patternNo list ((\(a,b) -> (x+a,y+b)) (getPattern patternNo))
 where char = fromMaybe 'Q' $ elementAt list ((\(a,b) -> (x+a,y+b)) (getPattern patternNo))
       updatedList = addCross list (x,y)

addChar :: Char -> [String] -> (Int, Int) -> [String]
addChar c list (x,y) = (\(a,b1:b2) -> a++(addCharLine c b1 x:b2)) $ splitAt y list

addCharLine :: Char -> String -> Int -> String
addCharLine c list x = (\(a,_:b) -> a++(c:b)) $ splitAt x list
