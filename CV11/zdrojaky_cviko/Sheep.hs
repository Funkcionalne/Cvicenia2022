module Sheep where
import Data.Maybe
import Control.Monad

-- a sheep has its name, and maybe mother and father
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}   deriving (Eq)

instance Show Sheep where
  show s = show (name s)

-- toto je priklad na Maybe monadu, ktora je definovana takto
-- instance Monad Maybe where
--    Nothing  >>= f = Nothing    -- `bind`
--    (Just x) >>= f = f x
--    return         = Just       -- return

------------------------------------------------------------------------------------

maternalGrandfather :: Sheep -> Maybe Sheep

-- klasicky: stary otec z matkinej strany
maternalGrandfather' o = if mother o == Nothing then
                            Nothing
                         else 
                            father (fromJust (mother o))

-- monadicky: stary otec z matkinej strany (aj do aj bind)
maternalGrandfather s = do
    m <- mother s
    father m

-- otca matky matka
fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do
    o <- father s
    m <- mother o
    mother m
    

weirdMonadicPlus a b = do
  x <- a
  y <- b
  return $ x + y
  
notSoWeirdApplicativePlus a b = pure (+) <*> a <*> b

-- matky otca otec
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = undefined

parents_ :: Sheep -> [Maybe Sheep]
parents_ x = undefined
-- parents_ dolly = [Nothing,Just "Molly"]

parents :: Sheep -> Maybe [Sheep]
parents x = undefined
-- parents dolly = Nothing

parents' :: Sheep -> [Sheep]
parents' x = undefined

--parents' x = (if father x == Nothing then [] else [fromJust (father x)])
--             ++
--             (if mother x == Nothing then [] else [fromJust (mother x)])

-- parents' dolly = ["Molly"]

-- convert a Maybe value into another monad
maybeToMonad :: (MonadPlus m) => Maybe a -> m a
maybeToMonad = undefined

-- parents'' dolly = Just ["Molly"]

---- nejake data:
adam   = Sheep "Adam"    Nothing Nothing
eve    = Sheep "Eve"     (Just eve) (Just roger)
uranus = Sheep "Uranus"  Nothing Nothing
gaea   = Sheep "Gaea"    Nothing Nothing
kronos = Sheep "Kronos"  (Just gaea) (Just uranus)
holly  = Sheep "Holly"   (Just eve) (Just adam)
roger  = Sheep "Roger"   (Just eve) (Just kronos)
molly  = Sheep "Molly"   (Just holly) (Just roger)
dolly  = Sheep "Dolly"   (Just molly) Nothing

main = parents_ dolly
