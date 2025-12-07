import System.IO
import Debug.Trace (trace, traceShowId)
import Data.List.Split (splitOn, chunksOf, splitWhen)
import Data.List (elemIndex, transpose)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, isNothing, fromJust, catMaybes)

main = do
        contents <- readFile "input.txt"
        let (vals, fns) = unzip
              . map (composeColumn . reverse)
              . transpose
              . lines $ contents
        print . sum $ zipWith execute (catMaybes fns) (splitOn [0] vals)

composeColumn' :: String -> Int
composeColumn' [] = 0
composeColumn' (x:xs)
        | x == ' ' = composeColumn' xs
        | otherwise = digitToInt x + 10 * composeColumn' xs

composeColumn :: String -> (Int, Maybe (Int -> Int -> Int, Int))
composeColumn (f:xs) = (composeColumn' xs, fn)
        where fn = case f of ' ' -> Nothing
                             '*' -> Just ((*), 1)
                             '+' -> Just ((+), 0)

execute :: (Int -> Int -> Int, Int) -> [Int] -> Int
execute (_, init) [] = init
execute (fn, init) (v:list) = fn v (execute (fn, init) list)

-- main = do
--         contents <- readFile "test.txt"
--         print . sum
--               . map (executeColumn . reverse)
--               . transpose
--               . map (filter (not . null) . splitOn " ")
--               . lines $ contents
-- 
-- executeColumn :: [String] -> Int
-- executeColumn (f:x:xs) = foldr (fn . read) (read x) xs
--         where fn = case f of "*" -> (*)
--                              "+" -> (+)
