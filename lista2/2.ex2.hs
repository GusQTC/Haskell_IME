data Caixa a = Um a | Dois a a | Tres a a a deriving (Show)

instance Functor Caixa where
    fmap f (Um a) = Um (f a)
    fmap f (Dois a b) = Dois (f a) (f b)
    fmap f (Tres a b c) = Tres (f a) (f b) (f c)

instance Applicative Caixa where
    pure = Um
    (Um f) <*> (Um a) = Um (f a)
    (Dois f g) <*> (Dois a b) = Dois (f a) (g b)
    (Tres f g h) <*> (Tres a b c) = Tres (f a) (g b) (h c)
    _ <*> _ = error "Invalid application"

instance Monad Caixa where
    return = Um
    -- return = pure ? 
    (Um a) >>= f = f a
    (Dois a b) >>= f = case f a of 
        Um x -> f b
        _ -> error "Invalid application"
        -- cant have anything other than Um x, as it should be Dois x y
    (Tres a b c) >>= f = case f a of
        Um x -> case f b of
            Um y -> f c
            _ -> error "Invalid application"
            -- Cant have anything other than Um x and Um y, as it should be Tres x y z


-- Part two

mult234 :: Double -> Caixa Double
mult234 x = Um x >>= (\a -> Tres (a*2) (a*3) (a*4))

--  equivalent to mult234 x = Tres (x*2) (x*3) (x*4)