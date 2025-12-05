import qualified Data.Set as S
import qualified Data.Map as M

solveDay5Part1 :: FilePath -> IO Int
solveDay5Part1 = solve part1

solveDay5Part2 :: FilePath -> IO Integer
solveDay5Part2 = solve part2

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

parseInput :: String -> ([(Integer, Integer)], [Integer])
parseInput input = (map parseRange ranges, map read ids)
 where (ranges,_:ids) = span (/= "") $ lines input

parseRange :: String -> (Integer, Integer)
parseRange range = (read min,read max)
 where (min,_:max) = span (/= '-') range

--Part 1
isFreshId :: [(Integer, Integer)] -> Integer -> Bool
isFreshId ranges id = any (\(min,max) -> id >= min && id <= max) ranges

countFreshIds :: [(Integer, Integer)] -> [Integer] -> Int
countFreshIds ranges = length . filter (isFreshId ranges)

part1 :: String -> Int
part1 input = uncurry countFreshIds $ parseInput input

--Part 2
createIntsCountsList :: M.Map (S.Set Int) (Integer, Integer) -> M.Map Int (Integer, Integer) -> [Integer]
createIntsCountsList intRngs baseRng
 | M.null nextInts = [intSum $ M.elems intRngs]
 | otherwise = intSum (M.elems intRngs):createIntsCountsList nextInts baseRng
 where nextInts = allInts baseRng intRngs

intSum :: [(Integer, Integer)] -> Integer
intSum ranges = sum $ map (\(min,max) -> max-min+1) ranges

allInts :: M.Map Int (Integer, Integer) -> M.Map (S.Set Int) (Integer, Integer) -> M.Map (S.Set Int) (Integer, Integer)
allInts baseRng = M.foldrWithKey (\parts rng ints -> M.union ints $ intOf parts rng baseRng) M.empty

intOf :: S.Set Int -> (Integer, Integer) -> M.Map Int (Integer, Integer) -> M.Map (S.Set Int) (Integer, Integer)
intOf parts rng = M.mapKeys (`S.insert` parts) . M.mapMaybe (intersection rng) . M.filterWithKey (\part _ -> S.notMember part parts)

intersection :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
intersection (min1,max1) (min2,max2)
 | min1 >= min2 && max1 <= max2 = Just (min1,max1)
 | min1 <= min2 && max1 >= max2 = Just (min2,max2)
 | min1 >= min2 && min1 <= max2 = Just (min1,max2)
 | min2 >= min1 && min2 <= max1 = Just (min2,max1)
 | otherwise = Nothing

sumInclExclnList :: Integer -> [Integer] -> Integer
sumInclExclnList _ [] = 0
sumInclExclnList one (x:xs) = one*x + sumInclExclnList (one*(-1)) xs

part2 :: String -> Integer
part2 input = sumInclExclnList 1 $ createIntsCountsList (M.mapKeys S.singleton baseRng) baseRng
 where baseRng = M.fromList $ zip [1..] $ fst $ parseInput input