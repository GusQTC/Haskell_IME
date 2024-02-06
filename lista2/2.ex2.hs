data Caixa a = Um a | Dois a a | Tres a a a deriving Show

instance Functor Caixa where
    fmap f (Um a) = Um (f a)
    fmap f (Dois a b) = Dois (f a) (f b)
    fmap f (Tres a b c) = Tres (f a) (f b) (f c)

instance Monad Caixa where
    return a = Um a
    (Um a) >>= f = f a
    (Dois a b) >>= f = Dois (f a) (f b)
    (Tres a b c) >>= f = Tres (f a) (f b) (f c)


mult234 :: Double -> Caixa Double
mult234 a = Tres (a*2) (a*3) (a*4)
mult234 (Dois a b) = Dois (a*2) (b*2)
mult234 (Um a) = Um (a*2)
mult234 _ = Um 0


