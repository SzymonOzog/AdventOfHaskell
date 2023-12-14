import Data.List
import Debug.Trace (traceShow)

findReflectionLine :: [String] -> Int -> Int -> Int
findReflectionLine pattern idx sumudges_left
    | length pattern == (idx + 1) = -1
    | validateReflectionLine pattern idx (idx+1) sumudges_left = idx+1
    | otherwise = findReflectionLine pattern (idx+1) sumudges_left

validateReflectionLine :: [String] -> Int -> Int -> Int -> Bool
validateReflectionLine pattern li ri smudges_left
    | li < 0 = smudges_left == 0
    | ri == length pattern = smudges_left == 0
    | pattern!!ri == pattern!!li = validateReflectionLine pattern (li-1) (ri+1) smudges_left
    | hammingDistance (pattern!!ri) (pattern!!li) == smudges_left = validateReflectionLine pattern (li-1) (ri+1) 0
    | otherwise = False

hammingDistance :: Eq a => [a] -> [a] -> Int
hammingDistance = (sum .) . zipWith ((fromEnum .) . (/=))

main :: IO()
main = do
    contents <- readFile "day13.txt"
    let patterns = filter (/=[""]) (groupBy (\x y -> y /= "" && x /= "") (lines contents))
    let summary = [if v /= -1 then v  else 100 * h | x<-patterns, let v = findReflectionLine (transpose x) 0 0, let h = findReflectionLine x 0 0]
    print (sum summary)
    let summary = [if v /= -1 then v  else 100 * h | x<-patterns, let v = findReflectionLine (transpose x) 0 1, let h = findReflectionLine x 0 1]
    print (sum summary)