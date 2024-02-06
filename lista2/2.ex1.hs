data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving Show

instance Functor Coisa where
    fmap f (UmaCoisa a) = UmaCoisa (f a)
    fmap f (DuasCoisas a b) = DuasCoisas (f a) (f b)
    fmap f ZeroCoisa = ZeroCoisa

-- 1.2
instance Applicative Coisa where
    pure a = UmaCoisa a
    (f) <*> (UmaCoisa a) = UmaCoisa (f a)
    (f g) <*> (DuasCoisas a b) = DuasCoisas (f a) (g b)
    _ <*> _ = ZeroCoisa

-- 1.3
-- mult234 :: Double -> Coisa Double 
--TODO !wrong
mult234:: Coisa -> Coisa Double
mult234 DuasCoisas a a = Applicative Coisa (a*2) (a*3)
mult234 UmaCoisa a = Applicative Coisa (a*2)


-- 1.d

data Arvore a = No a (Arvore a) (Arvore a) | Folha a deriving Show

instance Functor Arvore where
    fmap f (Folha a) = Folha (f a)
    fmap f (No a l r) = No (f a) (fmap f l) (fmap f r)

instance Applicative Arvore where
    pure a = Folha a
    (Folha f) <*> (Folha a) = Folha (f a)
    (No f l r) <*> (No a l' r') = No (f a) (l <*> l') (r <*> r')
    _ <*> _ = No undefined undefined undefined



