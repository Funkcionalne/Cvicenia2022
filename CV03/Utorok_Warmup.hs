import Data.List


-- definujte vlastnu analogiu pre zip
zips :: [Int] -> [Int] -> [Int]
zips [] [] = []
zips _ [] = []
zips [] _ = []
zips (x:xs) (y:ys) =  [x,y] ++ (zips xs ys) -- x:y:zips xs ys 

-- to iste pre tri zoznamy
zips3 :: [Int] -> [Int] -> [Int] -> [Int]
zips3 = undefined

-- to iste, pre lubovolny pocet zoznamov
zipsN :: [[t]] -> [[t]]
zipsN [] = []
zipsN ([]:_) = []
zipsN xss = [ head xs | xs <- xss] : zipsN (map tail xss)

-- to iste, pre lubovolny pocet zoznamov
zipsN' :: [[Int]] -> [[Int]]
zipsN' = undefined


-- a opacne, rozzipsovanie
unzips :: [Int] -> ([Int], [Int])
unzips [] = ([],[])
unzips (x:y:xs) = let (f, s) = unzips xs in (x:f,y:s)
-- (x: f, y: s) where (f, s) = unzips xs


unzips3 :: [Int] -> ([Int], [Int], [Int])
unzips3 = undefined

unzipsN :: [Int] -> Int -> [[Int]]
unzipsN [] _ = []
unzipsN xs n = take n xs : (unzipsN (drop n xs ) n)




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