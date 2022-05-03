-- jednokopovy nim
-- nim2 hraju dvaja hraci, zacina True, pokracuje False
-- z kopy striedavo beru 1,2 alebo 3 zapalky, kto berie poslednu, vyhrava
module Nim where

import Data.Char
import System.IO.Unsafe 
import System.Random
 
-- nahodne cislo 0..n-1
nextInt :: Int -> Int -> Int
nextInt a b = fromIntegral $ unsafePerformIO $ randomRIO (a, b-1) 

type Kopa = Int

-- kedy hra konci
finished :: Kopa -> Bool
finished = undefined

-- korektny tah
valid :: Kopa -> Int -> Bool
valid kopa num = undefined

getDigit :: IO Int
getDigit = do
    gv <- getLine
    let v = read gv
    return v

-- dvaja hraci
play2 :: Kopa -> Bool -> IO ()
play2 kopa hrac = undefined

-- dvaja hraci
nim2 :: IO ()
nim2 = play2 (nextInt 10 20) True

-- jeden hraci
strategia :: Int -> Int
strategia kopa = undefined
               

play1 :: Kopa -> IO ()
play1 kopa = do
   putStrLn $ "Kopa: " ++ (show kopa)
   putStrLn "Kolko zapaliek beries? "
   gv <- getDigit
   
   let novaKopa = kopa - gv
   
   putStrLn "koniec"

-- jeden hrac proti kompu
nim1 :: IO ()
nim1 = play1 (nextInt 10 20)
