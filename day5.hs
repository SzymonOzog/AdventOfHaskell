import Data.Char
import Data.Function
import Data.List

getMap :: [String] -> [(Int, Int, Int)]
getMap mapLines = map (\x -> (rInt (x!!0), rInt (x!!1),rInt (x!!2) )) w
    where
        w = map words mapLines

splitList :: [String] -> (String->Bool)-> [[String]]
splitList [] _ = []
splitList l pred = takeWhile pred l : splitList (drop 1 (dropWhile pred l)) pred

getLocation :: [(Int,Int)] -> [[(Int, Int, Int)]] -> Int
getLocation range maps = fst (minimumBy (compare `on` fst) outRanges)
    where
        outRanges = foldl getIdentifier range maps

extendSeeds :: [Int] -> [(Int,Int)]
extendSeeds [] = []
extendSeeds (x:x2:xs) = (x,x2) : extendSeeds xs

getIdentifier :: [(Int,Int)] -> [(Int, Int, Int)] -> [(Int,Int)]
getIdentifier range [] = range
getIdentifier [] _ = []
getIdentifier (x2:xs2) (x:xs) = next  ++ getIdentifier left xs ++ getIdentifier right xs ++ getIdentifier xs2 (x:xs)
    where
        next = getNextRange x2 x
        left = getLeftRemainder x2 x
        right = getRightRemainder x2 x

getNextRange :: (Int, Int) -> (Int,Int, Int) -> [(Int,Int)]
getNextRange (start, length) (a,b,c)
    | isInRange = [(a, c)]
    | isInRangeM = [(a+start-b, length)]
    | isInRangeL = [(a, length-(b-start))]
    | isInRangeR = [(a+start-b, c-(start-b))]
    | otherwise =  []
    where
        isInRange = (start<=b) && ((start+length)>=(b+c))
        isInRangeM = (start>=b) && ((start+length)<=(b+c))
        isInRangeL = ((start+length)>b) && (start<b)
        isInRangeR = (start>b) && (start<(b+c))

getLeftRemainder :: (Int, Int) -> (Int,Int, Int) -> [(Int,Int)]
getLeftRemainder (start, length) (a,b,c)
    | isOutOfRange = [(start,length)]
    | isInRangeL || isInRange = [(start,b-start)]
    | otherwise = []
    where
        isInRange = (start<=b) && ((start+length)>(b+c)) && (b-start)>0
        isInRangeL = start<b
        isOutOfRange = (start+length) < b

getRightRemainder :: (Int, Int) -> (Int,Int, Int) -> [(Int,Int)]
getRightRemainder (start, length) (a,b,c)
    | isOutOfRange = [(start,length)]
    | isInRangeR || isInRange = [(c+b, (start+length)-(c+b))]
    | otherwise = []
    where
        isInRangeR = (start+length) > (b+c) -- && (start<(b+c))
        isInRange = (start<=b) && ((start+length)>(b+c))
        isOutOfRange = start > (b+c)

rInt :: String -> Int
rInt = read
main :: IO ()
main = do
    contents <- readFile "day5.txt"
    let contentLines = lines contents
    let seeds = map rInt (drop 1 (words (head contentLines)))
    let all_maps = filter (/="") (drop 3 contentLines)
    let filteredMaps = map getMap (splitList all_maps (isDigit . head))
    let initialRanges = [(x,1)| x<-seeds]
    print (getLocation initialRanges filteredMaps)

    let extendedSeeds = extendSeeds seeds
    print (getLocation extendedSeeds filteredMaps)
