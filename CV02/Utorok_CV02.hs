module Utorok_CV02 where
import Test.QuickCheck
--import Text.Show.Functions
import Data.List

-- cabal update
-- cabal install QuickChech

-- pocet 3906
-- slova dlzky 3 nad abecedou
slova3 :: [Char] -> [String]
slova3 abeceda = [[ch1,ch2,ch3] | ch1 <- abeceda, ch2 <- abeceda, ch3 <- abeceda]


-- slova dlzky k nad abecedou
-- slova :: [Char] -> Int -> [[Char]]
-- type String = [Char]
slova :: String -> Int -> [String]
slova abeceda 0 = [""]
--slova abeceda 1 = [[ch] | ch<- abeceda]
slova abeceda k = [ch : w | w <-slova abeceda (k-1), ch<- abeceda]

pocetSlova :: [Char] -> Int -> Int 
pocetSlova abeceda k = (length abeceda)^k  

qchSlova = quickCheck(\abeceda -> \k -> (length abeceda < 6 && k >= 0 && k < 7)==>
            length (slova abeceda k) == pocetSlova abeceda k)

-- slova dlzky najviac k nad abecedou
slovaNajviac :: [Char] -> Int -> [String]
slovaNajviac abeceda 0 = [ "" ]
slovaNajviac abeceda k = [w | a <- [0..k], w <-slova abeceda a]


-- nub

{- length abeceda = q
1 + q + q^2 + ..... q^k  = Sk = ?
   q + q^2 + .....  q^k + q^(k+1)  = q.Sk

-1 + q^(k+1) = (q-1)*Sk
(-1 + q^(k+1))/(q-1) = Sk

-}
pocetSlovaNajviac :: String -> Int -> Int 
pocetSlovaNajviac [_] k = k+1
pocetSlovaNajviac abeceda k = let q = length abeceda in (q^(k+1) -1) `div` (q-1)

qchNajviac = quickCheck(\abeceda -> \k -> 
            (length abeceda < 6 && k >= 0 && k < 7)==>
            length (slovaNajviac abeceda k) == pocetSlovaNajviac abeceda k)

slovaNajviac' abeceda n =  undefined

qchNajviac' = undefined -- quickCheck()

qchNajviac'' = undefined -- quickCheck()


-- slova nad abecedou "ab", ktore neobsahuju aa, teda dve acka za sebou
-- ababbaa
slovaBezAA :: Int -> [String]
slovaBezAA 0 = [""]
slovaBezAA 1 = ["a","b"]
slovaBezAA k = ['b':w |  w <- slovaBezAA (k-1)] ++
                ['a':'b': w |  w <- slovaBezAA (k-2)]
               
-- ab................. (n-2)
-- b.................. (n-1)

qchSlovaBezAA = undefined -- quickCheck()


-- nemaju substring "aa"
slovaBezAAFilter n = filter (\w -> not(isInfixOf "aa" w)) $ slova "ab" n

qchSlovaBezAA' =  undefined -- quickCheck()


fibonacci 0 = 1
fibonacci 1 = 2
fibonacci k = fibonacci (k-1) + fibonacci (k-2)

fibonacci' abeceda 0 = 1
fibonacci' abeceda 1 = length abeceda
fibonacci' abeceda k = (length abeceda-1) * (fibonacci' abeceda (k-1) + fibonacci' abeceda (k-2))
    
-- slova neobsahujce "aa"
slovaBezAA' :: String -> Int -> [String]
------------------slovaBezAA' abeceda n | length abeceda < n = [ "" ]
slovaBezAA' _ 0 = [ "" ]
slovaBezAA' abeceda n = undefined

qchSlovaBezAA'' = quickCheck(\abeceda -> \n -> 
                            (length abeceda <= 6 && 
                             length abeceda >= n && 
                             length (nub abeceda) == length abeceda &&
                             n >= 0 && n <= 10) ==>
                           length(slovaBezAA' ('a':abeceda) n) == fibonacci' ('a':abeceda) n)

{- ako vybehnut na skody vysky n, ked mozem robit kroky dlzky 1,2
n = 4 => 5
0|1,2,3|4      "bbb"
0|1,2, |4      "bba"
0|1,  3|4      "bab"
0|  2,3|4      "abb"
0|  2, |4      "aba"

n = 5 => 8
0|1,2,3,4|5
0|1,2,3, |5
0|1,2,  4|5
0|1,  3,4|5
0|1,  3, |5
0|  2,3,4|5
0|  2,3, |5
0|  2,  4|5
0|  2,3,4|5

n = 6 => 13....
-}

izoAA2Schody :: String -> [Int]
izoAA2Schody w = [ index | (ch, index) <- zip w [1..], ch == 'b' ] 

izoSchody2AA :: Int ->[Int] -> String
izoSchody2AA n sch = [  if elem index sch then 'b' else 'a' | index <- [1..n] ]


