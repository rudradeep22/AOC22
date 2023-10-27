import System.IO
import Data.Char


main::IO()
main = do
    contents <- readFile "input.txt"
    let result = sum $ map priority $ map commonFinder $ formatter contents
    print result

formatter::String -> [(String, String)]
formatter str = map splitAtHalf $ lines str

splitAtHalf :: [a] -> ([a], [a])
splitAtHalf xs = splitAt (length xs `div` 2) xs

commonFinder::(String, String) -> Char
commonFinder (str1,str2) = head [x | x <- str1, x `elem` str2]

priority:: Char -> Int
priority x
    | x`elem`['A'..'Z'] = ord x - 38
    | x`elem`['a'..'z'] = ord x - 96
    | otherwise         = error "Haskell error not mine"