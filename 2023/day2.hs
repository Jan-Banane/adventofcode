{-# LANGUAGE OverloadedStrings #-}

import Data.Tuple
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Map as M

part1 :: FilePath -> IO Integer
part1 fileName = do
    text <- readFile fileName
    let res = sum $ M.keys $ M.filter isPossible $ M.mapKeys (\k -> (read (T.unpack k) :: Integer)) $ M.fromList $ map (bimap (T.drop 5) (T.drop 1) . T.break (== ':') . T.pack) $ lines text
    return res

part2 :: FilePath -> IO Integer
part2 fileName = do
    text <- readFile fileName
    let res = sum $ map (powerOfSet . T.drop 1 . snd . T.break (== ':') . T.pack) $ lines text
    return res

powerOfSet :: T.Text -> Integer
powerOfSet game = product (M.elems $ minCubes game)

isPossible :: T.Text -> Bool
isPossible game = M.null $ M.filterWithKey (\color num -> maybe False (num >) (M.lookup color maxCubes) ) (minCubes game)

minCubes :: T.Text -> M.Map T.Text Integer
minCubes game = foldl (M.unionWith max) M.empty (map handFull $ T.splitOn ";" game)

handFull :: T.Text -> M.Map T.Text Integer
handFull part = M.mapKeys T.strip $ M.map (read . T.unpack)  $ M.fromList $ map (swap . T.break (==' ') . T.strip) $ T.splitOn "," part

maxCubes :: M.Map T.Text Integer
maxCubes = M.fromList [("red",12),("green",13),("blue",14)]