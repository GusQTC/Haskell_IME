data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving Show

instance Functor Coisa where
    fmap f (UmaCoisa a) = UmaCoisa (f a)
    fmap f (DuasCoisas a b) = DuasCoisas (f a) (f b)
    fmap f ZeroCoisa = ZeroCoisa

-- 1.2
instance Applicative Coisa where
    pure = UmaCoisa
    (UmaCoisa f) <*> something = UmaCoisa (f (extract something))
        where extract (UmaCoisa a) = a

    (DuasCoisas f g) <*> something = DuasCoisas (f (extract something)) (g (extract something))
        where extract (UmaCoisa a) = a
              extract (DuasCoisas a _) = a
              extract ZeroCoisa = error "Cannot extract from ZeroCoisa"
    ZeroCoisa <*> _ = ZeroCoisa
-- 1.3
-- mult234 :: Double -> Coisa Double 
mult234 :: Double -> Coisa Double
mult234 a = (*) <$> DuasCoisas 2 3 <*> pure a
-- fmap the multiplication of 2 and 3 by a, then inserts in the context with pure



-- 1.d
-- need to check

data Arvore a = No a (Arvore a) (Arvore a) | Folha a deriving Show

instance Functor Arvore where
    fmap f (Folha a) = Folha (f a)
    fmap f (No a l r) = No (f a) (fmap f l) (fmap f r)

instance Applicative Arvore where
    pure = Folha
    (Folha f) <*> (Folha a) = Folha (f a)
    (No f l r) <*> (No a l' r') = No (f a) (l <*> l') (r <*> r')
    _ <*> _ = No undefined undefined undefined

-- example tree
example = No 1 (No 2 (Folha 3) (Folha 4)) (Folha 5)



