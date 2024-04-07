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

q2l :: (Queue a) -> [a]
q2l (Queue x y) = x ++ reverse y

instance Eq a => Eq (Queue a) where
    (Queue [] []) == (Queue [] []) = True
    (Queue f1 b1) == (Queue f2 b2) = (f1++(reverse b1))==(f2++(reverse b2))

instance Functor Queue where
    fmap f (Queue fr ba) = (Queue (map f fr) (map f ba))

translation :: Num b => b -> Queue b -> Queue b
translation n q = fmap (+ n) q

instance Applicative Queue where
    pure x = (Queue [x] [])
    fs <*> q = Queue ((q2l fs) <*> (q2l q)) [] --fs: una cola de funciones; q: una cola
    
instance Monad Queue where
    return x = (Queue [x] []) --devuelve un valor empaquetado en la monada (la cola)

    -- (>>=) :: m a -> (a -> m b) -> m b
    q >>= f = (Queue (concatMap (\qq -> q2l qq) (map f (q2l q))) [])

    --He escrito eso para que se vean los pasos mejor. Explicacion: 

    --Lo peor para mi es la f: f toma un valor a y devuelve "Queue a". Esto pasa para todas las monadas,
    --y la mejor forma de tomarselo es no pensar de momento y "hacer que las cosas cuadren". 
    --Sabiendo eso:

    --Queremos que si nos entra una cola, le podamos aplicar una funcion incomoda como esa y nos devuelva
    --otra cola. Para conseguirlo:

    --1. Se transforma la cola en lista y se aplica f a todos sus elementos. Esto nos deja una lista de colas
    --2. Queremos una cola: transformamos todas las colas de la lista en listas
    --3. Para construir una cola nos sirve una lista: concatenamos las listas (concatMap hace 2&3)
    --   y ponemos esa unica lista resultado como primer parametro de la constructora

    --Es engorroso. Hay que pasar q a lista primero porque si tenemos una lista de colas, podemos
    --hacer el concatMap. Si no hacemos ese paso, podemos hacer fmap f q, y eso nos deja una cola
    --de colas, que tendriamos que concatMapear, pero no he encontrado formas comodas de hacerlo sin
    --definir mas propiedades para la cola, asi que pasar por las listas (aunque parezca redundante)
    --puede que sea lo mas comodo al final.

    --Una forma mas limpia y equivalente (pero dificil) de hacerlo:
    --q >>= f = (Queue (q2l q >>= q2l . f) [])

kfilter :: (p -> Bool)  -> Queue p -> Queue p
kfilter f q = do
    v <- q
    if f v
    then return v
    else create

--Alternativa:

--kfilter :: (p -> Bool) -> Queue p -> Queue p
--kfilter f q = q >>= \v -> if f v then return v else create