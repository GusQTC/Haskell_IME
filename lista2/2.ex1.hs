data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving Show

instance Functor Coisa where
    fmap f (UmaCoisa a) = UmaCoisa (f a)
    fmap f (DuasCoisas a b) = DuasCoisas (f a) (f b)
    fmap f ZeroCoisa = ZeroCoisa


instance Applicative Coisa where
    pure a = UmaCoisa a
    (UmaCoisa f) <*> (UmaCoisa a) = UmaCoisa (f a)
    (DuasCoisas f g) <*> (DuasCoisas a b) = DuasCoisas (f a) (g b)
    _ <*> _ = ZeroCoisa

-- mult234 :: Double -> Coisa Double 


data Arvore a = No a (Arvore a) (Arvore a) | Folha a deriving Show

instance Functor Arvore where
    fmap f (Folha a) = Folha (f a)
    fmap f (No a l r) = No (f a) (fmap f l) (fmap f r)

instance Applicative Arvore where
    pure a = Folha a
    (Folha f) <*> (Folha a) = Folha (f a)
    (No f l r) <*> (No a l' r') = No (f a) (l <*> l') (r <*> r')
    _ <*> _ = No undefined undefined undefined



