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

c = push 3 (push 2 (push 1 create))

instance Eq a => Eq (Queue a) where
    (Queue [] []) == (Queue [] []) = True
    (Queue f1 b1) == (Queue f2 b2) = (f1++(reverse b1))==(f2++(reverse b2))


c1 = push 4 (pop (push 3 (push 2 (push 1 create))))
c2 = push 4 (push 3 (push 2 create))