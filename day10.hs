{-# LANGUAGE ParallelListComp #-}
import Data.List
import Data.Maybe (fromMaybe)
directions = [('|', (1,0), (-1,0)),
              ('-', (0,1), (0,-1)),
              ('L', (-1,0), (0,1)),
              ('J', (-1,0), (0, -1)),
              ('7', (1,0), (0, -1)),
              ('F', (1,0), (0, 1))]

takeStep :: (Int, Int) -> (Int,Int) -> [String] -> (Int,Int)
takeStep (pr, pc) (cr,cc) tileMap = (cr+r, cc+c)
    where
       nextTile = tileMap!!cr!!cc 
       (r,c) = getNext nextTile (pr-cr, pc-cc) 

getNext :: Char ->(Int,Int) -> (Int,Int)
getNext current entryPoint 
    | i1 == entryPoint = i2
    | i2 == entryPoint = i1 
    | otherwise = (0,0)
    where
        (_, i1, i2) = fromMaybe (' ', (0,0), (0,0)) (find (\(c,_,_) -> current == c) directions)

findDistance :: (Int,Int) -> [String] -> Int
findDistance startingPos tileMap = stepAll startingPoints initialPositions tileMap 1  
    where
        (sr,sc) = startingPos
        allPossiblePos = [ (sr+r, sc+c) | (r,c)<-[(1,0),(-1,0),(0,1), (0,-1)]]
        posInRange = filter (\(r,c) -> (r>=0)&&(c>=0)) allPossiblePos
        posInRangeNotDot = filter (\(r, c) -> (tileMap!!r!!c) /= '.') posInRange 
        initialPositions = filter (canEnter tileMap startingPos) posInRangeNotDot
        startingPoints = [startingPos | x<-initialPositions]

canEnter :: [String] -> (Int,Int) -> (Int,Int) -> Bool
canEnter tileMap (pr,pc) (cr, cc) = (i1==next)||(i2==next)
    where
        current = tileMap!!cr!!cc
        (_,i1,i2) = fromMaybe (' ', (0,0), (0,0)) (find (\(c,_,_) -> current == c) directions)
        next = (pr-cr, pc-cc)


stepAll :: [(Int,Int)] -> [(Int,Int)] -> [String] -> Int -> Int
stepAll previousPos currentPos tileMap steps  
    | loopFound = steps+1 
    | otherwise = stepAll currentPos nextPos tileMap (steps + 1)
    where
        nextPos = filter (\(r, c) -> (tileMap!!r!!c) /= '.') [takeStep p c tileMap | p<-previousPos | c<-currentPos]
        loopFound = length nextPos /= length (nub nextPos)

getAnimalLoc :: [String] -> (Int,Int)
getAnimalLoc content = (r,c)
    where
        r = fromMaybe 0 (findIndex ('S' `elem`) content)
        c = fromMaybe 0 (findIndex (=='S') (content!!r))

main :: IO ()
main = do
    contents <- readFile "day10.txt"
    let animalLoc =  getAnimalLoc (lines contents) 
    print (findDistance animalLoc (lines contents))
