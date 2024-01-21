-- 5

data Nat = Z | Suc Nat deriving (Show)

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (Suc n) = 1 + natToInt n

natToIntList :: [Nat] -> [Int]
natToIntList = map natToInt

inttonat :: Int -> Nat
inttonat 0 = Z
inttonat n = Suc (inttonat (n - 1))

somar :: Nat -> Nat -> Nat
somar x Z = x
somar x (Suc n) = Suc (somar x n)

addLista :: Nat -> [Nat] -> [Nat]
addLista n [] = [n]
addLista n (x : xs) = (x : xs) ++ [n]

mult :: Nat -> Nat -> Nat
mult x Z = Z
mult x (Suc Z) = x
mult x (Suc n) = somar x (mult x n)

fatorial :: Nat -> Nat
fatorial Z = Suc Z
fatorial (Suc n) = mult (Suc n) (fatorial n)

fibonacci :: Nat -> Nat
fibonacci Z = Z
fibonacci (Suc Z) = Suc Z
fibonacci (Suc (Suc n)) = somar (fibonacci n) (fibonacci (Suc n))

fibonacciList :: Nat -> [Nat]
fibonacciList Z = [Z]
fibonacciList (Suc Z) = [Z, Suc Z]
fibonacciList (Suc (Suc n)) = addLista (somar (fibonacci n) (fibonacci (Suc n))) (fibonacciList (Suc n))

-- fibonacci 5 = [0, 1, 1, 2, 3]
-- use natToIntList to convert to Int
-- natToIntList (fibonacciList inttonat n)
-- 36 and over is stack overflow