-- Binary tree

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2  

size :: Tree a -> Int
size Empty = 0
size (Node a l r) = 1 + size l + size r

height :: Tree a -> Int
height Empty = 0
height (Node a l r) = 1 + max (height l) (height r)

equal :: Eq a => Tree a -> Tree a -> Bool 
equal Empty Empty = True
equal (Node a1 l1 r1) (Node a2 l2 r2) = a1==a2 && equal l1 l2 && equal r1 r2
equal _ _ = False

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a l r) = [a] ++ preOrder l ++ preOrder r

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst t = bfs [t]
    where
        bfs [] = []
        bfs (Empty:xs) = bfs xs
        bfs ((Node a l r):xs) = [a] ++ bfs (xs ++ [l] ++ [r])