data Tree a = Empty | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show (Empty) = "()"
    show (Node n l r) = "(" ++ show l ++ "," ++ show n ++ "," ++ show r ++ ")"

instance Functor (Tree) where
    fmap f Empty = Empty
    fmap f (Node n l r) = (Node (f n) (fmap f l) (fmap f r)) 

doubleT :: Num a => Tree a -> Tree a
doubleT t = fmap (*2) t

data Forest a = Forest [Tree a] deriving (Show)

instance Functor (Forest) where
    fmap f (Forest a) = Forest (map (fmap f) a)

doubleF :: Num a => Forest a -> Forest a
doubleF f = fmap (*2) f