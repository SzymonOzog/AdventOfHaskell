import Data.List
import Data.Maybe
goOnce :: String -> String -> [(String,(String,String))] -> Int -> (String, Int)
goOnce [] node _ steps = (node, steps)
goOnce (x:xs) node nodeMap steps = goOnce xs nextNode nodeMap (steps+1)
    where
        nextNode = takeStep nodeMap x node

takeStep :: [(String, (String,String))] -> Char -> String -> String
takeStep nodeMap instruction node = next (snd currentLine)
    where
        currentLine = fromMaybe ("", ("","")) (find (\x -> fst x == node) nodeMap)
        next = if instruction == 'L' then fst else snd

goUntil :: String -> [(String,(String,String))] -> (String, Int) ->((String,Int) -> Bool) -> Int
goUntil instructions nodeMap result cond
    | cond result = snd result
    | otherwise = goUntil instructions nodeMap newResult cond
    where
        newResult = goOnce instructions (fst result) nodeMap (snd result)

goMultiplePaths :: [String] -> String -> [(String,(String,String))] -> Int -> Int
goMultiplePaths startingNodes instructions nodeMap steps
    | finished = steps + 1
    | otherwise = goMultiplePaths nextNodes instructions nodeMap (steps+1)
    where
        x = instructions!!(steps `mod` length instructions)
        nextNodes = map (takeStep nodeMap x) startingNodes
        finished = length startingNodes == length (filter (\x -> (x!!2)=='Z') nextNodes)


main :: IO ()
main = do
    contents <- readFile "day8.txt"
    let instructions = head (lines contents)
    let nodeLines = drop 2 (lines contents)
    let nodeMap = [(node,(left,right)) | x <- nodeLines, let node = head (words x), let left = take 3 (drop 1 (words x!!2)), let right = take 3 (words x!!3)]
    print (goUntil instructions nodeMap ("AAA", 0) (\x -> fst x == "ZZZ"))

    let startingNodes = map fst (filter (\(x,(_,_)) -> (x!!2) =='A') nodeMap)
    let paths = map (\x -> goUntil instructions nodeMap (x, 0) (\x -> ((fst x)!!2) == 'Z')) startingNodes
    let greatestCommonDivisor =  foldr1 gcd paths
    let greatestCommonFactor a b = (a*b) `div` greatestCommonDivisor
    print (foldr1 greatestCommonFactor paths)
