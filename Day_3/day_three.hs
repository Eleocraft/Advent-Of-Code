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
insertJolt x r [n] = [if r >= 0 then max n x else n]
insertJolt x r (n:n2:xn)
                  | r == 0 = max x n : n2 : xn
                  | x > n2 = (-1) : insertJolt x (r - 1) (n2:xn)
                  | x > n = x : n2 : xn
                  | otherwise = n : n2 : xn


-- calculateBankPwr :: Int -> Int -> [Int] -> Int
-- calculateBankPwr fst snd [x] = fst * 10 + max x snd
-- calculateBankPwr fst snd (x:xs)
--                | x > fst = calculateBankPwr x (-1) xs
--                | x > snd = calculateBankPwr fst x xs
--                | otherwise = calculateBankPwr fst snd xs
