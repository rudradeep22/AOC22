import System.IO
import Data.List

-- Add a new line before first line in input.txt

main :: IO ()
main = do  
    contents <- readFile "input.txt"
    let result = sum $ take 3 $ reverse $ sort $ map sum $ map stringToIntList $ splitByEmptyLines contents
    print result

splitByEmptyLines :: String -> [String]
splitByEmptyLines str = go (lines str) []
  where
    go [] acc = acc
    go (x:xs) [] = go xs [x]
    go (x:xs) acc@(a:as)
      | null x = go xs ([]:acc)
      | otherwise = go xs ((a ++ " " ++ x):as)

stringToIntList :: String -> [Int]
stringToIntList str = map read (words str)