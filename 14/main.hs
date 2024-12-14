import Data.List.Split 
import Data.Map (Map, insert, empty, member, (!), adjust, elems, foldr, fromList)

data Robot = Rb { px :: Int
            , py :: Int
            , vx :: Int
            , vy :: Int
        } deriving (Show) 

cFIELD_WIDTH = 101
cFIELD_HEIGHT = 103
half_width = floor $ (fromIntegral cFIELD_WIDTH) / 2 :: Int
half_height = floor $ (fromIntegral cFIELD_HEIGHT) / 2 :: Int



data Quadrant = TL | TR | BL | BR | None
    deriving (Show, Eq)


part1 = do
    contents <-  readFile "s.txt"
    let robots = map parseRobot $ lines contents
    let moved = map (doFor 100 moveRobot) robots
    print moved
    let qmap = map getQuadrant moved
    print qmap
    let factor = safetyFactor qmap
    print factor

part2 = do
    contents <-  readFile "l.txt"
    let robots = map parseRobot $ lines contents
    step 0 robots

step :: Int -> [Robot] -> IO()
step i rbs = do
    let moved = map moveRobot rbs
    let gg = insertRobots emptyGrid moved
    --printGrid gg
    print i
    if (not $ isSymmetric gg) then
        step (i+1) moved
    else do
        print i 
        printGrid gg
        return ()

    c <- getChar
    if c /= 'c' then
        step (i+1) moved
    else
        return ()

sampleBot = (Rb 2 4 2 (-3))

emptyGrid = fromList [((x, y), '.') | x <- [0..cFIELD_WIDTH -1], y <- [0..cFIELD_HEIGHT-1]]


printGrid :: (Map (Int, Int) Char) -> IO()
printGrid grid = do
    let l = [[grid ! (x, y) | x <- [0..cFIELD_WIDTH-1]] | y <- [0..cFIELD_HEIGHT-1]]
    let ll = map ('\n' : ) l
    putStr $ unlines l

insertRobots :: (Map (Int, Int) Char) -> [Robot] -> (Map (Int, Int) Char)
insertRobots m [] = m
insertRobots m ((Rb x y _ _):rbs) = insertRobots updatedMap rbs
    where updatedMap = insert (x, y) '*' m

isSymmetric :: (Map (Int, Int) Char) -> Bool
isSymmetric grid = all (True==) [(grid ! (x, y)) == (grid ! (cFIELD_WIDTH - 1 - x, y)) | x <- [0..half_width], y <- [0..cFIELD_HEIGHT -1]]


doFor :: Int -> (Robot -> Robot) -> Robot -> Robot
doFor 0 _ rb = rb
doFor n f rb = doFor (n-1) f (f rb)

wrapValue :: Int -> Int -> Int
wrapValue max val 
    | val >= max = wrapValue max (val - max)
    | val < 0 = wrapValue max (max + val)
    | otherwise = val

getQuadrant :: Robot -> Quadrant
getQuadrant (Rb x y _ _)
        | x < hx && y < hy = TL
        | x < hx && y > hy = BL
        | x > hx && y < hy = TR
        | x > hx && y > hy = BR
        | otherwise = None
      where 
        hx = half_width
        hy = half_height


safetyFactor :: [Quadrant] -> Int
safetyFactor qs = qsum $ helper qs (0, 0, 0, 0)
    where 
        helper :: [Quadrant] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
        helper [] tp = tp
        helper (q:qs) (tl, tr, bl, br)
            | q == TL = helper qs (tl + 1, tr, bl, br)
            | q == TR = helper qs (tl, tr + 1, bl, br)
            | q == BL = helper qs (tl, tr, bl + 1, br)
            | q == BR = helper qs (tl, tr, bl, br + 1)
            | otherwise = helper qs (tl, tr, bl, br)

        qsum :: (Int, Int, Int, Int) -> Int
        qsum (a, b, c, d) = a * b * c * d

wrapAround :: Robot -> Robot
wrapAround (Rb px py vx vy) = (Rb nx ny vx vy)
    where
        nx = wrapValue cFIELD_WIDTH px
        ny = wrapValue cFIELD_HEIGHT py

moveRobot :: Robot -> Robot
moveRobot (Rb px py vx vy) = wrapAround (Rb (px + vx) (py + vy) vx vy)

parseRobot :: String -> Robot
parseRobot s = do
    let sp = splitOn " " s
    let (px, py) = do
            let vec = splitOn "=" (sp !! 0)
            let sp' = splitOn "," (vec !! 1)
            (toInt $ sp' !! 0, toInt $ sp' !! 1)

    let (vx, vy) = do
            let vec = splitOn "=" (sp !! 1)
            let sp' = splitOn "," (vec !! 1)
            (toInt $ sp' !! 0, toInt $ sp' !! 1)
        
    (Rb px py vx vy)

toInt :: String -> Int
toInt = read