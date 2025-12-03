import System.IO
import Debug.Trace (trace)
import Data.List.Split (splitOn, chunksOf)

main = do
        contents <- readFile "input.txt"
        print . sum . map (uncurry sumInvalid . readRange) . splitOn "," $ contents

readRange :: String -> (Int, Int)
readRange s = 
    let (a, b) = break (== '-') s
    in (read a, read $ drop 1 b)

sumInvalid :: Int -> Int -> Int
sumInvalid pos end 
  | pos > end = 0
  | otherwise = (if isInvalid pos 2 then trace ("pos: " ++ show pos) pos else 0) + sumInvalid (pos + 1) end

isInvalid :: Int -> Int -> Bool
isInvalid v m
  | m <= len = (mod len m == 0 && same (chunksOf (div len m) strInt)) || isInvalid v (m + 1)
  | otherwise = False
  where same [x] = False
        same (x:xs) = all (==x) xs
        len = length strInt
        strInt = show v

-- First task
-- isInvalid :: Int -> Bool
-- isInvalid v = same (splitAt (div len 2) strInt)
--   where same (a, b) = a == b
--         len = length strInt
--         strInt = show v
