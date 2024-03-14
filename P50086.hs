-- Cua 2

-- Queue

data Queue a = Queue [a] [a]
    deriving (Show)
 
create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue f b) = Queue f $ [x]++b

pop :: Queue a -> Queue a
pop (Queue [] []) = Queue [] []
pop (Queue [] b) = pop (Queue (reverse b) [])
pop (Queue (f:fs) b) = (Queue fs b)

top :: Queue a -> a
top (Queue [] b) = top (Queue (reverse b) [])
top (Queue (f:fs) b) = f

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a) where
    (Queue [] []) == (Queue [] []) = True
    (Queue f1 b1) == (Queue f2 b2) = (f1++(reverse b1))==(f2++(reverse b2))

instance Functor (Queue a) where
    fmap f (Queue fr ba) = (Queue (map f fr) (map f ba))

--translation :: Num b => b -> Queue b -> Queue b
