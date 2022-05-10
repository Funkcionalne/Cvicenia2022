module Rhododendron where
import Control.Monad.State

data Tree a = Node a [Tree a]  deriving (Show, Eq)

e1 :: Tree String
e1 = Node "Jano" [    (Node "Fero"  [(Node "a" []),(Node "b" []),(Node "a" []),(Node "b" [])]),
            (Node "Jano"  [(Node "a" [])]),
            (Node "Karel"  [(Node "c" []),(Node "a" []),(Node "c" [])]),
            (Node "Fero"  [(Node "d" []),(Node "b" []),(Node "a" []),(Node "c" [])]),
            (Node "Karel"  [(Node "d" []),(Node "a" []),(Node "d" [])])
        ]
        
-- spocita velkost stromu - len preto, aby ste si vyskusali rekurziu na Rhododendrone, alias Tree a.    
size :: Tree a -> Int
size (Node _ sons) = 1 + (sum $ map size sons)

    
-- vytvori identicky strom, pricom hodnoty vrcholov su postupne iduce prirodzene cisla 0,1,...
-- na sposobe prechadzania stromu nezalezi    
reindex :: Tree a -> Tree Int
reindex t = evalState (reindex' t) 0
        where
            reindex' :: Tree a -> State Int (Tree Int)
            reindex' (Node _ sons) = do i <- get
                                        put (i+1)
                                        newSons <- mapM reindex' sons
                                        return (Node i newSons)
{-
reindex e1 =
Node 0 [         (Node 1  [(Node 2 []),(Node 3 []),(Node 4 []),(Node 5 [])]),
                           (Node 6  [(Node 7 [])]),
                           (Node 8  [(Node 9 []),(Node 10 []),(Node 11 [])]),
                           (Node 12  [(Node 13 []),(Node 14 []),(Node 15 []),(Node 16 [])]),
                           (Node 17  [(Node 18 []),(Node 19 []),(Node 20 [])])
                  ]
-}

-- vytvori identicky strom, pricom hodnoty vrcholov su prirodzene cisla 0,1,...
-- s tym dolezitym rozdielom, ze rovnake hodnoty typu ::a maju rovnake indexy, cisla

type Table a = [a]
rename  :: (Eq a) => Tree a -> Tree Int
rename t = evalState (rename' t) []
      where
          rename' :: (Eq a) => Tree a -> State (Table a) (Tree Int)
          rename' (Node root sons) = do rootIndex <- numberNode root
                                        newSons <- mapM rename' sons
                                        return (Node rootIndex newSons)
          numberNode :: (Eq a) => a -> State (Table a) Int
          numberNode x = do table <- get     
                            (newTable, newPos) <- return (addNode x table)
                            put newTable
                            return newPos
          addNode::  (Eq a) => a -> Table a -> (Table a, Int)
          addNode x table = case (findIndexInList (== x) table) of
                                Nothing -> (table ++ [x], length table)
                                Just i  -> (table, i)
          findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
          findIndexInList = findIndexInListHelp 0
          findIndexInListHelp _ _ [] = Nothing
          findIndexInListHelp count f (h:t) = if (f h)
                                              then Just count
                                              else findIndexInListHelp (count+1) f t

{-
rename e1 =
Node 0 [         (Node 1  [(Node 2 []),(Node 3 []),(Node 2 []),(Node 3 [])]),
                           (Node 0  [(Node 2 [])]),
                           (Node 4  [(Node 5 []),(Node 2 []),(Node 5 [])]),
                           (Node 1  [(Node 6 []),(Node 3 []),(Node 2 []),(Node 5 [])]),
                           (Node 4  [(Node 6 []),(Node 2 []),(Node 6 [])])
                  ]  -- cislovanie sa moze lisit od vasej implementacie
-}
