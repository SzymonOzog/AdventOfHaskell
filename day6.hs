{-# LANGUAGE ParallelListComp #-}
import Data.Char

rInt :: String -> Int
rInt = read

findWinningTimes :: (Int,Int) -> Int
findWinningTimes (duration, best) = high - low + 1
    where
        delta = fromIntegral ((duration^2) - (4*best))
        eps = 1e-10
        low = ceiling (((fromIntegral duration - sqrt delta) / 2) + eps)
        high = floor (((fromIntegral duration + sqrt delta) / 2) - eps)

main :: IO ()
main = do
    contents <- readFile "day6.txt"
    print contents
    let timeLine = head (lines contents)
    let distanceLine = last (lines contents)

    let times = map rInt (words (drop 6 timeLine))
    let distances = map rInt (words (drop 11 distanceLine))
    let timeToDistance = [(t,d) | t <- times| d<- distances]
    print (product (map findWinningTimes timeToDistance))

    let parseRec = rInt . filter isDigit
    let timeDistance = (parseRec timeLine, parseRec distanceLine)
    print (findWinningTimes timeDistance)