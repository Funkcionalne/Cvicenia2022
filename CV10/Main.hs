module Main where

data MyMaybe a = MyJust a | MyNothing  deriving (Show) -- alias Maybe a

instance Functor MyMaybe where
 fmap f (MyJust s ) = MyJust (f s) 
 fmap f MyNothing = MyNothing

data MyList a = Null | Cons a (MyList a) deriving (Show) -- alias [a]


instance Functor MyList where
 fmap f (Cons a ( bs)) = (Cons (f a) (fmap f ( bs) )) 
 fmap f Null = Null
 
data HumanSnoc a = Nil | Snoc (HumanSnoc a) a deriving Show -- alias opacne komponovany [a]
 
data GeekySnoc a = GeekyNil | (GeekySnoc a ) :< a deriving Show -- alias opacne komponovany [a]

instance Functor GeekySnoc where
 fmap f (a :< b) =    ((fmap f (a) ) :< (f b) ) 
 fmap f GeekyNil = GeekyNil
                                                             -- inspiracia - ondrejova otazka z prednasky

main :: IO ()
main = putStrLn ":<"
