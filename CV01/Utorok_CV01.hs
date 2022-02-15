module Utorok_CV01 where

import Data.List


main = do
  putStrLn (show (fact'' 5))
  putStrLn (show $ pocetNulFakt 1000)
  putStrLn (show $ (pocetMoznosti 10000))

-- show :: a -> String
-- type String = [Char]
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

fact' :: Integer -> Integer
fact' n | n < 2 = 1
        | otherwise = n * fact' (n-1)

fact'' :: Integer -> Integer
fact'' n = if n < 2 then 1 else n * fact'' (n-1)

pocetCifier :: Integer -> Integer
pocetCifier n | n < 10 = 1
              | otherwise = 1+ pocetCifier (n `div` 10)


pocetNul :: Integer -> Integer 
pocetNul 0 = 1
pocetNul n | n < 10 = 0
           | (n `mod` 10) == 0 = 1 + (pocetNul (n `div` 10)) 
           | otherwise = pocetNul (n `div` 10)

pocetNul' :: Integer -> Integer 
pocetNul' 0 = 1
pocetNul' n | n < 10 = 0
            | n `mod` 10 == 0 = 1 + pom
            | otherwise = pom
            where pom = pocetNul' (n `div` 10)

pocetKNul :: Integer -> Integer
pocetKNul n | n == 0 = 1
            | n < 10 = 0
            | otherwise = if n `mod` 10 == 0 then 1 + pocetKNul (n `div` 10) else 0

pocetCifier' :: Integer -> Integer
pocetCifier' n = toInteger (length (show n))

-- log 11 = 1....
pocetCifier'' :: Integer -> Integer
--pocetCifier'' n = 1+floor (logBase 10 (fromIntegral n))
pocetCifier'' n = ceiling (logBase 10 (fromIntegral (1+n)))

hypoteza1 = filter (\n -> not (pocetCifier n == pocetCifier'' n)) [1..100000]

-- 125 =3
-- 25 = 2
-- 30 = 1
-- 13 = 0
log5 :: Int -> Int
log5 0 = 0
log5 n = if n `mod` 5 == 0 then 1+log5(n `div` 5) else 0


pocetNulFakt :: Int -> Int
pocetNulFakt n = sum (map log5 [1..n])

--1*2*3*4*5*6*7*8*9*10*11*(2*2*3)...n = XXXXX0000
-- 2*5 = 10

pocetMoznosti :: Int -> Int
pocetMoznosti n = length [ 7 | a <- delitele n, b <- delitele n, c <- delitele n, a*b*c == n]

delitele n = [ d | d <- [1..n], n `mod` d == 0]