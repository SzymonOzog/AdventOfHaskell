{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Maybe
import Data.Function (on)
-- import Data.Tup)
cards = "J23456789TQKA"

rInt :: String -> Int
rInt = read

cardStrength :: Char -> Int
cardStrength card = fromMaybe 0 (elemIndex card cards)

isStronger :: (String, Int) -> (String, Int) -> Ordering
isStronger lhs rhs  
    | typeStrengthL == typeStrengthR = compareOrdering (fst lhs) (fst rhs)
    | otherwise = compare typeStrengthL typeStrengthR
    where
        typeStrengthL = getTypeStrength (fst lhs)
        typeStrengthR = getTypeStrength (fst rhs)

getTypeStrength :: String -> Int
getTypeStrength hand = (6 - length g) + biggestGroup
    where
        newHand = map (\c -> if c == 'J' then mostFrequent hand else c) hand
        g = group (sort newHand)
        biggestGroup = maximum (map length g)

mostFrequent :: String -> Char
mostFrequent "JJJJJ" = 'J'
mostFrequent hand = head (maximumBy (compare `on` length) (group (sort (filter (/='J') hand))))

compareOrdering :: String -> String -> Ordering
compareOrdering (x:xs) (x2:xs2) 
    | x == x2 = compareOrdering xs xs2
    | otherwise = compare (cardStrength x) (cardStrength x2)


main :: IO ()
main = do
    contents <- readFile "day7.txt"
    let l = lines contents
    let handToBid = [(hand,bid) | x <- l, let hand = head (words x), let bid = rInt (last (words x))]
    let sortedHands = sortBy isStronger handToBid
    let winnings = [rank * bid | (_, bid) <- sortedHands | rank <-[1..]]
    print (sum winnings) 