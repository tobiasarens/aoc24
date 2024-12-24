import Data.Map as Map
import Data.List.Split (splitOn)
import Data.List as List
import Data.Set as Set
import Prelude as Pr

data Logic = AND String String | OR String String | XOR String String
    deriving (Show)


p1 = do
    contents <- readFile "l.txt"
    (values, gates) <- parseInput contents
    --print values
    --print gates

    let zs = List.sort . getZs $ Map.keys gates
    print zs

    let v' = Pr.foldl (\m s -> snd (computeValue (m, gates) s)) values zs :: Map String Bool

    --print v'
    let vZs = Pr.map ((!) v') zs
    print vZs

    let num = bintodec vZs
    print num

p2 = do
    contents <- readFile "s.txt"
    (values, gates) <- parseInput contents
    let zs = List.sort . (getVars 'z') $ Map.keys gates
    let xs = List.sort . (getVars 'x') $ Map.keys values
    let ys = List.sort . (getVars 'y') $ Map.keys values

    let xyz = zip zs $ zip xs ys
    let carry = zip (List.drop 1 zs) $ zip xs ys
    let toCheck =  xyz ++ carry

    let wrong = Pr.map fst . Pr.filter (\(z, (x, y)) -> not (isReachable gates z x ) || not (isReachable gates z y)) $ toCheck
    print wrong

    let sSpace =  searchSpace gates wrong
    print $ length sSpace
    print $ length (getPerms sSpace)

    print $ computeCorrect (values, gates) (xs, ys, zs) sSpace

    print ""

computeCorrect :: (Map String Bool, Map String Logic) -> ([String], [String], [String]) -> [String] -> [(String, String)]
computeCorrect (values, gates) (x,y,z) sspace = helper (values, gates) (getPerms sspace)
    where
        helper :: (Map String Bool, Map String Logic) -> [[(String, String)]] -> [(String, String)]
        helper _ [] = []
        helper (values, gates) (p:perm) = if (checkGates values' (x,y,z)) then p else (helper (values, gates) perm)
            where
                gates' = swap gates p
                values' = Pr.foldl (\m s -> snd (computeValue (m, gates') s)) values z :: Map String Bool

checkGates :: Map String Bool -> ([String], [String], [String]) -> Bool
checkGates values (xs, ys, zs) = x + y == z
    where
        x = bintodec . Pr.map (values !) $ xs
        y = bintodec . Pr.map (values !) $ ys
        z = bintodec . Pr.map (values !) $ ys


swap :: Map String Logic -> [(String, String)] -> Map String Logic
swap gates [] = gates
swap gates ((a,b):ss) = swap gates' ss
    where
        ga = gates ! a
        gb = gates ! b
        gates' = Map.insert a gb . Map.insert b ga $ gates


getPerms :: [String] -> [[(String, String)]]
getPerms gates = [[(a, b), (c,d), (e,f), (g,h)] | a <- el, b <- el, a < b, 
                                                    c <- el, c /= a, c /= b,
                                                    d <- el, c < d,
                                                    e <- el, e /= c, e /= d, e /= a, e/= b,
                                                    f <- el, e < f,
                                                    g <- el, g /= e, g /= f, g /= c, g /= d, g /= a, g /= b,
                                                    h <- el, g < h]
    where
        el = gates

searchSpace :: Map String Logic -> [String] -> [String]
searchSpace gates s = helper gates s Set.empty
    where
        helper gates [] seen = Set.toList seen
        helper gates (x:xs) seen
            |  Set.member x seen = helper gates xs seen
            | not (Map.member x gates) = helper gates xs seen'
            | otherwise = helper gates xs' seen'
            where
                seen' = Set.insert x seen
                (p1, p2) = getParents (gates ! x)
                xs' = p1:p2:xs


contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) a = x==a || contains xs a

isReachable :: Map String Logic -> String -> String -> Bool
isReachable gates start end 
    | start == end = True
    | not (Map.member start gates) = False
    | otherwise = any (\x -> isReachable gates x end) parents
    where
        parents = getParents (gates ! start)

getParents :: Logic -> (String, String)
getParents gate = case gate of
        (OR a b) -> (a, b)
        (AND a b) -> (a, b)
        (XOR a b) -> (a, b)

getZs :: [String] -> [String]
getZs = Pr.filter (\(s:ss) -> s == 'z')

getVars :: Char -> [String] -> [String]
getVars c = Pr.filter (\(s:ss) -> s == c)

computeValue :: (Map String Bool, Map String Logic) -> String -> (Bool, Map String Bool)
computeValue (values, gates) val = if Map.member val values then (values ! val, values)
                            else
                                (comp, values')
                where
                    (comp, v') = computeGate (values, gates) (gates ! val)
                    values' = Map.insert val comp values

computeGate :: (Map String Bool, Map String Logic) -> Logic -> (Bool, Map String Bool)
computeGate (values, gates) (AND a b) = (va && vb, values')
        where
            (va, v) = computeValue (values, gates) a
            (vb, values') = computeValue (v, gates) b
computeGate (values, gates) (OR a b) = (va || vb, values')
        where
            (va, v) = computeValue (values, gates) a
            (vb, values') = computeValue (v, gates) b
computeGate (values, gates) (XOR a b) = (xor va vb, values')
        where
            (va, v) = computeValue (values, gates) a
            (vb, values') = computeValue (v, gates) b

bintodec :: [Bool] -> Int
bintodec = Pr.foldr (\x y -> fromEnum x + 2*y) 0


parseInput :: String -> IO (Map String Bool, Map String Logic)
parseInput contents = do
    let sp = splitOn [""] $ lines contents
    let values = parseValues (sp !! 0)
    let gates = parseGates (sp !! 1)
    return (values, gates)
    where
        parseValues :: [String] -> Map String Bool
        parseValues [] = Map.empty
        parseValues (x:xs) = Map.insert var val (parseValues xs)
            where
                sp = splitOn ":" x
                var = (sp !! 0) 
                val = toBool (sp !! 1) :: Bool

        parseGates :: [String] -> Map String Logic
        parseGates [] = Map.empty
        parseGates (x:xs) = Map.insert var val $ parseGates xs
            where
                sp = splitOn " -> " x
                var = sp !! 1
                val = parseGate (sp !! 0)

parseGate :: String -> Logic
parseGate s = do
    let sp = splitOn " " s
    let v1 = sp !! 0
    let v2 = sp !! 2
    case (sp !! 1) of
        "OR" -> OR v1 v2
        "AND" -> AND v1 v2
        "XOR" -> XOR v1 v2

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

toBool :: String -> Bool
toBool s = let i = read s :: Int in 
        i > 0