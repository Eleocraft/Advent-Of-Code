import System.IO
import Debug.Trace

main = do
        contents <- readFile "input.txt"
        print . getNumZeros 50 . map readMove . lines $ contents

readMove :: String -> Int
readMove (h:t) = if h == 'L' then negate num else num 
    where num = read t

getNumZeros :: Int -> [Int] -> Int
getNumZeros _ [] = 0
getNumZeros x (m:rest) = countZero x m + getNumZeros newPos rest
        where newPos = mod (x + m) 100
              countZero x m
                  | m < 0 = isMod (x - 1) + countZero (mod (x - 1) 100) (m + 1)
                  | m > 0 = isMod (x + 1) + countZero (mod (x + 1) 100) (m - 1)
                  | otherwise = 0
              isMod x = if mod x 100 == 0 then 1 else 0

-- first part of exersise
-- getNumZeros :: Int -> [Int] -> Int
-- getNumZeros _ [] = 0
-- getNumZeros x (n:rest) = ifZero newPos + getNumZeros newPos rest
--         where newPos = mod (x + n) 100
--               ifZero y = if y == 0 then 1 else 0
