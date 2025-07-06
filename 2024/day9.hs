import Data.Char (digitToInt)
import Data.Maybe

--Common Funcs
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

prepareDisk :: [Int] -> [(Int,Int)]
prepareDisk = prepareSingle 0

prepareSingle :: Int -> [Int] -> [(Int,Int)]
prepareSingle _ [] = []
prepareSingle id (x:xs)
 | odd id = (x,-1):prepareSingle (id+1) xs
 | otherwise = (x, div id 2):prepareSingle (id+1) xs

generateDisk ::[(Int,Int)] -> [Int]
generateDisk [] = []
generateDisk ((a,b):xs) =  replicate a b ++ generateDisk xs

calculateChecksum :: [Int] -> Int
calculateChecksum disk = sum $ map (uncurry (*)) $ filter (\(num,d) -> num /= -1) $ zip disk  [0..]

--Part 1
part1 :: String -> Int
part1 = calculateChecksum . compress . generateDisk . prepareDisk . map digitToInt

compress :: [Int] -> [Int]
compress disk = take (length disk) $ compression (length disk) disk (reverse disk)

compression :: Int -> [Int] -> [Int] -> [Int]
compression len (x:xs) (y:ys)
 | len > (1+length xs + length ys) = repeat (-1)
 | x > -1 = x:compression len xs (y:ys)
 | y > -1 = y:compression len xs ys
 | otherwise = compression len (x:xs) ys

--Part 2
part2 :: String -> Int
part2 = calculateChecksum . generateDisk . reverse . generateCompressedDisk . reverse . prepareDisk . map digitToInt

generateCompressedDisk :: [(Int,Int)] -> [(Int,Int)]
generateCompressedDisk ((a,b):reversed)
 | b == -1 = (a,b):generateCompressedDisk reversed
 | otherwise = insertFile (a,b) reversed
generateCompressedDisk [] = []

insertFile :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertFile (a,b) reversed =
 case compressFile (a,b) (reverse reversed) of
    Nothing -> (a,b):generateCompressedDisk reversed
    Just result -> (a,-1):generateCompressedDisk (reverse result)

compressFile :: (Int,Int) -> [(Int,Int)] -> Maybe [(Int,Int)]
compressFile _ [] = Nothing
compressFile (a,b) ((c,d):list)
 | d == -1 && a <= c = Just ((a,b):(c-a,-1):list)
 | otherwise = fmap ((c,d):) (compressFile (a,b) list)