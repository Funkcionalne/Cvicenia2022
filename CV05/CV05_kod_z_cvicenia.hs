module Main where

import Terms (LExp(..), Var)
import Data.List

instance Show LExp where
  show (ID x) = x 
  show (LAMBDA x ex) = '\\': x ++ "->" ++ show ex
  show (APP el1 el2) = '(': show el1 ++ " " ++ show el2 ++ ")"

free :: LExp -> [String]
free (ID x) = [x]
free (APP x y) = free x ++ free y
free (LAMBDA x e) = filter (/= x) (free e)

subterms :: LExp -> [LExp]
subterms a@(ID _) = [a]
subterms a@(LAMBDA _ b) = a : subterms b
subterms a@(APP e1 e2) = a : (subterms e1 ++ subterms e2)

exp1 :: LExp
exp1 = LAMBDA "x" (APP (ID "y") (LAMBDA "z" (ID "m")))

exp2 :: LExp
exp2 = LAMBDA "y" (APP (ID "x") (ID "x"))

exp3 :: LExp
exp3 = APP (ID "x") (LAMBDA "x" (ID "x"))

main :: IO ()
main = do
  print exp1
  print exp2
  print exp3
