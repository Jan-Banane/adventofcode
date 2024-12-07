import Data.Maybe

solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

part1 input = length $ concatMap (filter (=='X')) $ moveUp (lines input) $ findGuard input

moveUp :: [String] -> (Int,Int) -> [String]
moveUp list (x,y)
 |(isNothing test) = (addCross list (x,y))
 | Just True == test = moveRight list (x,y)
 | otherwise = moveUp (addCross list (x,y)) (x,y-1)
 where test = fmap (=='#') $ elementAt list (x,y-1)

moveRight :: [String] -> (Int,Int) -> [String]
moveRight list (x,y)
 |(isNothing test) = (addCross list (x,y))
 | Just True == test = moveDown list (x,y)
 | otherwise = moveRight (addCross list (x,y)) (x+1,y)
 where test = fmap (=='#') $ elementAt list (x+1,y)
 
moveDown :: [String] -> (Int,Int) -> [String]
moveDown list (x,y)
 |(isNothing test) = (addCross list (x,y))
 | Just True == test = moveLeft list (x,y)
 | otherwise = moveDown (addCross list (x,y)) (x,y+1)
 where test = fmap (=='#') $ elementAt list (x,y+1)
 
moveLeft :: [String] -> (Int,Int) -> [String]
moveLeft list (x,y)
 |(isNothing test) = (addCross list (x,y))
 | Just True == test = moveUp list (x,y)
 | otherwise = moveLeft (addCross list (x,y)) (x-1,y)
 where test = fmap (=='#') $ elementAt list (x-1,y)

addCross :: [[Char]] -> (Int, Int) -> [[Char]]
addCross list (x,y) = (\(a,b1:b2) -> a++(addCrossLine b1 x:b2)) $ splitAt y list

addCrossLine :: [Char] -> Int -> [Char]
addCrossLine list x = (\(a,_:b) -> a++('X':b)) $ splitAt x list

elementAt :: [String]-> (Int, Int) -> Maybe Char
elementAt text (x,y)
 | 0 <= x && x < length (head text) && 0 <= y && y < length text = Just (text !! y !! x)
 | otherwise = Nothing

findGuard input = (x,y)
    where (before,_) = break (=='^') input
          y = (length $ lines before) -1
          x = length $ last $ lines before

