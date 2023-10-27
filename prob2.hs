import System.IO
import Data.List

main :: IO()
main = do
    contents <- readFile "input.txt"
    let result = sum $ map score $ formatter contents
    print result

formatter::String -> [[String]]
formatter str = map words $ lines str

score::[String] -> Int
score str
    | first == "A" && second == "X" = 4
    | first == "A" && second == "Y" = 8
    | first == "A" && second == "Z" = 3
    | first == "B" && second == "X" = 1
    | first == "B" && second == "Y" = 5
    | first == "B" && second == "Z" = 9
    | first == "C" && second == "X" = 7
    | first == "C" && second == "Y" = 2
    | first == "C" && second == "Z" = 6
    | otherwise                     = 0

    where first = head str
          second = last str
