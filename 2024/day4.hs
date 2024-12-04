import Data.Maybe (catMaybes)

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

mapOnEachElem :: ([String] -> (Int, Int) -> Int) -> [Char] -> Int
mapOnEachElem func fileContent = sum $ map (func text) $ indexesToAllElem (length $ head text) (length text)
    where text = lines fileContent

indexesToAllElem :: Int -> Int -> [(Int, Int)]
indexesToAllElem lenX lenY = concatMap (\y -> map (,y) [0..lenX-1]) [0..lenY-1]

elementAt :: [String]-> (Int, Int) -> Maybe Char
elementAt text (x,y)
 | 0 <= x && x < length (head text) && 0 <= y && y < length text = Just (text !! y !! x)
 | otherwise = Nothing

--Part 1
part1 :: String -> Int
part1 = mapOnEachElem countXmas

countXmas :: [String] -> (Int, Int) -> Int
countXmas list (x,y)
 | list !! y !! x == 'X' = countSurroundingForMas list (x,y)
 | otherwise = 0

countSurroundingForMas :: [String] -> (Int, Int) -> Int
countSurroundingForMas text xy = length $ filter (checkLineForMas text xy) [(1,0),(-1,0),(0,1),(0,-1),(1,1),(-1,1),(1,-1),(-1,-1)]

checkLineForMas :: [String] -> (Int, Int) -> (Int, Int) -> Bool
checkLineForMas text (x,y) (a,b) =
    catMaybes [elementAt text (x+(1*a),y+(1*b)), elementAt text (x+(2*a),y+(2*b)), elementAt text (x+(3*a),y+(3*b))] == "MAS"

--Part 2
part2 :: String -> Int
part2 = mapOnEachElem countXmas'

countXmas' :: [String] -> (Int, Int) -> Int
countXmas' text (x,y)
 | text !! y !! x == 'A' && length ( filter (isMas text (x,y)) [(1,1),(1,-1),(-1,1),(-1,-1)]) >= 2 = 1
 | otherwise = 0

isMas :: [String] -> (Int, Int) -> (Int, Int) -> Bool
isMas text (x,y) (a,b) = Just 'M' == elementAt text (x+a,y+b) && Just 'S' == elementAt text (x-a,y-b)