module Main where

import System.Random
import System.IO.Unsafe
import Control.Monad.State.Lazy

data MyMaybe a = MyJust a | MyNothing  deriving (Show) -- alias Maybe a

instance Functor MyMaybe where
 fmap f (MyJust s ) = MyJust (f s) 
 fmap f MyNothing = MyNothing
 
instance Applicative MyMaybe where
	pure  =  MyJust
	MyNothing <*> _ = MyNothing
	(MyJust f) <*> gs = fmap f gs 
	
instance Monad MyMaybe where
	return = pure
	MyNothing >>= _ = MyNothing 
	(MyJust f) >>= g =  g f
	
zdvoj :: [Int] -> [Int]
zdvoj xs = concat[[x,x]|x <- xs]

zdvoj' :: [Int] -> [Int]
zdvoj' xs = do
    x <- xs
    [x, x]

zdvoj'' :: [Int] -> [Int]
zdvoj'' xs = xs >>= (\x -> [x, x])

monadMap :: Monad m => (a -> b) -> m a -> m b
monadMap f xs = do
    x <- xs
    pure $ f x


data MyList a = Null | Cons a (MyList a) deriving (Show) -- alias [a]


instance Functor MyList where
 fmap f (Cons a bs) = Cons (f a) (fmap f bs) 
 fmap f Null = Null
 
data HumanSnoc a = Nil | Snoc (HumanSnoc a) a deriving Show -- alias opacne komponovany [a]

 
data GeekySnoc a = GeekyNil | (GeekySnoc a) :< a deriving Show -- alias opacne komponovany [a]

instance Functor GeekySnoc where
 fmap f (a :< b) =    fmap f a :< f b  
 fmap f GeekyNil = GeekyNil
                                                             -- inspiracia - ondrejova otazka z prednasky
data M1 a = Raise String | Return a deriving Show

pureRandomR :: Int -> Int -> Int
pureRandomR a b = unsafePerformIO $ randomRIO (a, b)

impureRandomR :: Int -> Int -> IO Int
impureRandomR = curry randomRIO


main :: IO ()
main = do
  putStrLn "PURE RANDOM"

  print $ pureRandomR 1 10
  print $ pureRandomR 1 10
  print $ pureRandomR 1 10
  print $ pureRandomR 1 10
  print $ pureRandomR 1 10

  putStrLn "IO RANDOM"

  impureRandomR 1 10 >>= print
  impureRandomR 1 10 >>= print
  impureRandomR 1 10 >>= print
  impureRandomR 1 10 >>= print
  impureRandomR 1 10 >>= print

