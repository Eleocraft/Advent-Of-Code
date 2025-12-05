import System.IO
import Debug.Trace (trace)
import Data.List.Split (splitOn, chunksOf)
import Data.Char (digitToInt)

main = do
        contents <- readFile "input.txt"
        print . sum . map (calculateBankPwr (replicate 12 (-1)) . parse) . lines $ contents

parse :: String -> [Int]
parse = map digitToInt

calculateBankPwr :: [Int] -> [Int] -> Int
calculateBankPwr [] [] = 0
calculateBankPwr (n:xn) [] = n + 10 * calculateBankPwr xn []
calculateBankPwr num (x:xs) = calculateBankPwr (insertJolt x (length xs) num) xs

insertJolt :: Int -> Int -> [Int] -> [Int]
insertJolt x r [n] = [max n x]
insertJolt x r (n:nn:xn)
                  | r == 0 = max x n : nn : xn
                  | x > nn = (-1) : insertJolt x (r - 1) (nn:xn)
                  | x > n = x : nn : xn
                  | otherwise = n : nn : xn

-- First exercise
-- calculateBankPwr :: Int -> Int -> [Int] -> Int
-- calculateBankPwr fst snd [x] = fst * 10 + max x snd
-- calculateBankPwr fst snd (x:xs)
--                | x > fst = calculateBankPwr x (-1) xs
--                | x > snd = calculateBankPwr fst x xs
--                | otherwise = calculateBankPwr fst snd xs
