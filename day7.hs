{-# LANGUAGE ParallelListComp #-}
import Data.List
import Data.Maybe
cards = "23456789TJQKA"

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
        g = group (sort hand)
        biggestGroup = maximum (map length g)

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
    print ""