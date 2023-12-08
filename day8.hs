import Data.List
import Data.Maybe
import Debug.Trace (traceShow)
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

goUntil :: String -> [(String,(String,String))] -> (String, Int) -> Int
goUntil instructions nodeMap result
    | fst result == "ZZZ" = snd result
    | otherwise = goUntil instructions nodeMap newResult
    where
        newResult = goOnce instructions (fst result) nodeMap (snd result)



main :: IO ()
main = do
    contents <- readFile "day8.txt"
    let instructions = head (lines contents)
    let nodeLines = drop 2 (lines contents)
    let nodeMap = [(node,(left,right)) | x <- nodeLines, let node = head (words x), let left = take 3 (drop 1 (words x!!2)), let right = take 3 (words x!!3)]
    print (goUntil instructions nodeMap ("AAA", 0))
