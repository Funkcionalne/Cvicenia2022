import Data.List


-- definujte vlastnu analogiu pre zip
zips :: [Int] -> [Int] -> [Int]
zips = undefined

-- to iste pre tri zoznamy
zips3 :: [Int] -> [Int] -> [Int] -> [Int]
zips3 = undefined

-- to iste, pre lubovolny pocet zoznamov
zipsN :: [[Int]] -> [Int]
zipsN = undefined

-- to iste, pre lubovolny pocet zoznamov
zipsN' :: [[Int]] -> [[Int]]
zipsN' = undefined


-- a opacne, rozzipsovanie
unzips :: [Int] -> ([Int], [Int])
unzips = undefined

unzips3 :: [Int] -> ([Int], [Int], [Int])
unzips3 = undefined

unzipsN :: [Int] -> Int -> [[Int]]
unzipsN xs n = undefined




myReverse :: [t] -> [t]
myReverse xs = foldl (\acc x -> x:acc) [] xs 

nub' :: [Int] -> [Int]
nub' xs = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] xs

duplikuj :: [Int] -> [Int]
duplikuj xs = foldr (\x acc -> x:x:acc) [] xs

dropEvery :: [Int] -> Int -> [Int]
dropEvery xs n = foldr (\(i, x) acc -> if i `mod` n == 0 then acc else x:acc) [] $ zip [1..] xs

zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = foldr (\i acc -> (xs!!i, ys!!i):acc) [] [0..len-1]
          where len = minimum [(length xs), (length ys)]