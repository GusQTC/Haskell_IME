data Caixa a = Um a | Dois a a | Tres a a a deriving Show

instance Functor Caixa where
    fmap f (Um a) = Um (f a)
    fmap f (Dois a b) = Dois (f a) (f b)
    fmap f (Tres a b c) = Tres (f a) (f b) (f c)




