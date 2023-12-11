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

findDistanceAndLast :: (Int,Int) -> [String] -> (Int, [(Int,Int)])
findDistanceAndLast startingPos tileMap = stepAll startingPoints initialPositions tileMap 1
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


stepAll :: [(Int,Int)] -> [(Int,Int)] -> [String] -> Int -> (Int, [(Int,Int)])
stepAll previousPos currentPos tileMap steps
    | loopFound = (steps+1, head nextPos : [head currentPos])
    | otherwise = stepAll currentPos nextPos tileMap (steps + 1)
    where
        nextPos = filter (isValid tileMap) [takeStep p c tileMap | p<-previousPos | c<-currentPos]
        loopFound = length nextPos /= length (nub nextPos)

isValid :: [String] -> (Int,Int) -> Bool
isValid tileMap (r,c) = (tileMap!!r!!c) /= '.'

getAnimalLoc :: [String] -> (Int,Int)
getAnimalLoc content = (r,c)
    where
        r = fromMaybe 0 (findIndex ('S' `elem`) content)
        c = fromMaybe 0 (elemIndex 'S' (content!!r))

reconstructPath :: [String] -> [(Int,Int)] -> [(Int,Int)]
reconstructPath tileMap current
 | tileMap!!r!!c =='S' = current
 | otherwise = reconstructPath tileMap (current++[nextPos])
 where
    numTiles = length current
    nextPos = takeStep (current!!(numTiles-2)) (current!!(numTiles-1)) tileMap
    (r,c) = nextPos

makeLines :: [(Int,Int)] -> [[(Int,Int)]]
makeLines [x] = []
makeLines (x:x2:xs) = [x, x2] : makeLines (x2:xs)

intersectsLine :: [(Int,Int)] -> (Int,Int) -> Bool
intersectsLine [(xr,xc), (x2r,x2c)] (r,c)
    | (xr == x2r) && (r == xr) = (c >= min xc x2c) && (c <= max xc x2c)
    | (xc == x2c) && (c == xc) = (r >= min xr x2r) && (r <= max xr x2r)
    | otherwise = False

rightOf :: (Int,Int) -> [(Int,Int)] -> Bool
rightOf (r,c) [(l1r, l1c),(l2r,l2c)]
    | not isVerticalLine = intersectsLine [(l1r,l1c), (l2r,l2c)] (r,c)
    | c < l1c = False
    | (r > max l1r l2r) || (r<=min l1r l2r) = False
    | otherwise = c>=l2c
    where
        isVerticalLine = l2c == l1c

downOf ::  (Int,Int) -> [(Int,Int)] -> Bool
downOf (r,c) [(l1r, l1c),(l2r,l2c)]
    | isVerticalLine = intersectsLine [(l1r,l1c), (l2r,l2c)] (r,c) 
    | r < l1r = False
    | (c > max l1c l2c) || (c<=min l1c l2c) = False
    | otherwise = r>=l2r
    where
        isVerticalLine = l2c == l1c

isInside :: [[(Int,Int)]] -> (Int,Int) -> Bool
isInside pathLines p = (rightNum `mod`2 == 1) && (downNum `mod` 2 == 1) && not anyIntersecting
    where
        rightNum = length (filter (rightOf p) pathLines)
        downNum =  length (filter (downOf p) pathLines)
        anyIntersecting = any (`intersectsLine` p) pathLines

main :: IO ()
main = do
    contents <- readFile "day10.txt"
    let tileMap = lines contents
    let animalLoc =  getAnimalLoc tileMap
    let distanceAndLast = findDistanceAndLast animalLoc tileMap
    print (fst distanceAndLast)
    let takenPath = reconstructPath  tileMap (snd distanceAndLast) ++ [animalLoc] ++ reverse (drop 2 (reconstructPath  tileMap (reverse (snd distanceAndLast))))

    let verts =  filter (\(x,y) -> not ((tileMap!!x!!y == '|') || (tileMap!!x!!y == '-'))) takenPath
    let pathLines = makeLines verts ++ [[head verts, last verts]]

    let totalR = length tileMap
    let totalC = length (head tileMap)

    let allPoints = concat [[(r,c) | c<-[0..totalC]] | r<- [0..totalR]]
    let insideLines = isInside pathLines
    print ( length (filter insideLines allPoints))