
{-# LANGUAGE OverloadedStrings     #-}

import Control.Applicative (Alternative((<|>)))
import Data.Char (isNumber)
import Data.Maybe ( catMaybes )
import Text.Read (readMaybe)
import Data.Text as T

solveDay3Part1 :: FilePath -> IO Int
solveDay3Part1 = solve part1

solveDay3Part2 :: FilePath -> IO Int
solveDay3Part2 = solve part2 

--Common Functions
solve :: (String -> a) -> FilePath -> IO a
solve solver fileName = do
    text <- readFile fileName
    return $ solver text

parseMul :: T.Text -> Maybe Int
parseMul
    txt = do
    textWoMul <- T.stripPrefix "mul(" txt
    num1 <- tryParse $ T.unpack (T.take 3 textWoMul)
    let textWoNum1 = T.dropWhile isNumber textWoMul
    textWoComa <- T.stripPrefix "," textWoNum1
    num2 <- tryParse $ T.unpack (T.take 3 textWoComa)
    let textWoNum2 = T.dropWhile isNumber textWoComa
    textWoClosingBracket <- T.stripPrefix ")" textWoNum2
    return (num1 * num2)

tryParse :: String -> Maybe Int
tryParse [a,b,c] =
    readMaybe [a,b,c] <|> readMaybe [a,b] <|> readMaybe [a]
tryParse _ = Nothing

--Part 1
part1 :: String -> Int
part1 = sum . catMaybes . searchMul . T.pack

searchMul :: T.Text -> [Maybe Int]
searchMul txt
 | T.length txt >= 1 = parseMul txt:searchMul (T.tail txt)
 | otherwise = []

--Part 2
part2 :: String -> Int
part2 = sum . catMaybes . searchDontAndMul . T.pack

searchDontAndMul :: Text -> [Maybe Int]
searchDontAndMul txt
 | T.length txt >= 1 = if T.isPrefixOf "don't()" txt then searchDo $ T.tail txt else parseMul txt:searchDontAndMul (T.tail txt)
 | otherwise = []

searchDo :: Text -> [Maybe Int]
searchDo txt
 | T.length txt >= 1 = if T.isPrefixOf "do()" txt then searchDontAndMul $ T.tail txt else searchDo $ T.tail txt
 | otherwise = []