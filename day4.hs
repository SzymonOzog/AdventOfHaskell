{-# LANGUAGE ParallelListComp #-}
import Data.List

countWonCopies :: Int -> [Int] -> [Int] -> [Int]
countWonCopies i won acc 
    | i == length won = acc
    | otherwise = countWonCopies (i+1) won newAcc
    where 
        currentNum = acc !! i
        newAcc = [if y>i && y <= i+(won!!i) then x+currentNum  else x | x <- acc | y<-[0..]]

main :: IO ()
main = do
    contents <- readFile "day4.txt"
    let contetnLines = lines contents
    let filtered = map (drop 1 . dropWhile (/= ':')) contetnLines
    let winningToYour = [ (words w, words (drop 1 y))| x <- filtered, let (w,y) = span (/= '|') x ]
    let won = [length (y `intersect` w) | (w,y) <- winningToYour]
    let points = [2 ^ (x-1) | x <- filter (>0) won]
    print (sum points)

    let cardsWon = foldl (\acc w -> acc ++ [w]) [] won
    let currentCards = take (length contetnLines) (repeat 1)
    print (sum(countWonCopies 0 won currentCards))