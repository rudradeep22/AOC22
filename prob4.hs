import System.IO
main::IO()
main = do
    contents <- readFile "input.txt"
    let result = length $ filter (==True) $ map check $ map extractNumbers $ formatter contents
    print result

formatter str = lines str

splitNumbers :: String -> [String]
splitNumbers [] = []
splitNumbers (x:xs)
    | x `elem` "-," = splitNumbers xs
    | otherwise = let (num, rest) = span (`notElem` "-,") (x:xs)
                  in num : splitNumbers rest

extractNumbers :: String -> [Int]
extractNumbers str = map read (splitNumbers str)

check::[Int] -> Bool
check (a:b:c:d:_)
    | (a-c)*(b-d) <= 0 = True
    | otherwise        = False
