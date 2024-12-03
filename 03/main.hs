import Text.Regex.TDFA
import Data.Char

mulRegex :: String
mulRegex = "mul\\([0-9]+,[0-9]+\\)"

numberRegex :: String
numberRegex = "[0-9]+"

part1 = do
    contents <- readFile "l.txt"
    let results = getAllTextMatches (contents =~ mulRegex) :: [String]

    let result = map (\x -> getAllTextMatches (x =~ numberRegex) :: [String]) results
    let numbers = [map toInt i | i <- result]

    let final = sum $ map calc numbers

    print final

part2 = do
    contents <- readFile "l.txt"

    let all = enabled contents

    let inp = foldr (++) "" all :: String

    let results = getAllTextMatches (inp =~ mulRegex) :: [String]

    let result = map (\x -> getAllTextMatches (x =~ numberRegex) :: [String]) results
    let numbers = [map toInt i | i <- result]
    let final = sum $ map calc numbers

    print final


enabled :: String -> [String]
enabled "" = []
enabled s = en: disabled ds
    where (en, _, ds) = s =~ "don't\\(\\)" :: (String, String, String)

disabled :: String -> [String]
disabled s = enabled en
    where (_, _, en) = s =~ "do\\(\\)" :: (String, String, String)

calc :: [Int] -> Int
calc [x, y] = x*y

toInt :: String -> Int
toInt = read

trd :: (a, b, c) -> c
trd (_, _, a) = a

