module Utorok_CV08_Unify where
import Data.Char

data Term = Var String | 
            CN Int | 
            Functor String [Term]  -- deriving(Show)

instance Show Term where
    show (CN n) = show n
    show (Var name) = name
    show (Functor f args) = f ++ if null args then [] 
                                 else 
                                   "(" ++ (show (args!!0))++ (concat [ "," ++ (show (args!!i))  | i <- [1..length args-1] ]) ++ ")" 

type Constraint = (Term, Term)       -- term1 == term2
type Constraints = [Constraint]

unify :: Constraints -> Maybe Constraints
unify [] = Just []
unify ((Functor f fargs, Functor g gargs) :cs) 
                        | f == g && length fargs == length gargs = unify ([(fargs!!i, gargs!!i) | i <- [0.. (length fargs) - 1]] ++ cs)
                        | otherwise = Nothing
unify (x@(Var name, t)) : cs = if occurs name t then Nothing else unify (x : substitute name t cs)
unify (x@(t, Var name)) : cs = if occurs name t then Nothing else unify (x : substitute name t cs)
unify (CN i, CN j) : cs = if i==j then unify cs else Nothing
unify x:cs = Nothing

e1 = Functor "f" [ Var "X", Var "X" ]               -- f (x, x)
e2 = Functor "f" [ CN 5,    Var "Y" ]               -- f (5, y)
e3 = Functor "f" [ CN 5,    CN 6 ]                  -- f (5, 6)
e4 = Functor "f" [ (Functor "g" [Var "Y"]),    Var "Y" ]                  -- f (g(y), y)
e5 = Functor "f" [ (Functor "g" [Var "Z"]),    Var "Y" ]                  -- f (g(z), y)
e6 = Functor "f" [ (Functor "g" [Var "Z"]),   (Functor "g" [CN 7]) ]      -- f (g(z), g(7))

ee = [
      (Var "U", Functor "f" [Var "V", Var "V"]),
      (Var "Z", Functor "f" [Var "U", Var "U"]),
      (Var "Y", Functor "f" [Var "Z", Var "Z"]),
      (Var "X", Functor "f" [Var "Y", Var "Y"])
      ]


-- unify [(e1, e2)] = Just [(x,5),(y,5)]
-- unify [(e1, e3)] = Nothing
-- unify [(e1, e4)] = Nothing
-- unify [(e1, e5)] = Just [(x,g(z)),(y,g(z))]
-- unify [(e1, e6)] = Just [(x,g(z)),(z,7)]

occurs :: String -> Term -> Bool
occurs x (Var name) = x == name
occurs x (CN y) = False
occurs x (Functor f fargs) = any (\farg -> occurs x farg) fargs
-- occurs "x" e1
-- occurs "y" e1

substitute :: String -> Term -> Term -> Term   -- var t t1 = t2, ak t1[x:t]
substitute = undefined
-- substitute "x" (Var "y") e1

substitute' :: String -> Term -> Constraint -> Constraint
substitute' name t (c1, c2) = (substitute name t c1, substitute name t c2)

substitute'' :: String -> Term -> Constraints -> Constraints
substitute'' name t cs = map (\a -> substitute' name t a) cs

-- prida prvok do Maybe List
add2Maybe :: Constraint -> Maybe Constraints -> Maybe Constraints
add2Maybe = undefined
 

fromStringArgs  :: String -> ([Term], String)
fromStringArgs xs = let (a, xs') = fromString xs in 
                       if not (null xs') &&  head xs' == ',' then let (as,xs'') = fromStringArgs (tail xs') in ((a:as), xs'')
                       else ([a], tail xs')


fromString  :: String -> (Term, String)
fromString (x:xs)   | isDigit x  = (CN (ord x-48), xs)
fromString [x]      | isAlpha x && isLower x  = (Functor [x] [], [])
fromString (x:y:xs) | isAlpha x && isLower x && y == '(' = let (args, xs') = fromStringArgs xs in (Functor [x] args, xs')
fromString (x:xs)   | isAlpha x && isUpper x  = (Var [x], xs)
fromString  xs      = error ("syntax error: " ++ xs)                                                   
