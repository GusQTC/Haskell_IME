import System.IO
import Distribution.SPDX (LicenseId(X11))

data Nat = Z | Suc Nat deriving (Show)

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (Suc n) = 1 + natToInt n

somar :: Nat -> Nat -> Nat
somar x Z = x
somar x (Suc n) = Suc (somar x n)


addLista :: Nat -> [Nat] -> [Nat]
addLista n [] = [n]
addLista n (x:xs) = (x:xs) ++ [n]


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
--  0, 1, 1, 2, 3, 5, 8, 13, 21, 34
-- Path: test.hs

data Func = Kons Double
    | X
    | Power Func Int
    | Sen Func
    | Cos Func
    | Exp Func
    | Add Func Func
    | Mult Func Func


ddx :: Func -> Func
ddx (Kons _) = Kons 0
ddx X = Kons 1
ddx (Power X n) = Mult (Kons (fromIntegral n)) (Power X (n-1))
ddx (Power f n) = Mult (Mult (Kons (fromIntegral n)) (Power f (n-1))) (ddx f)
ddx (Sen f) = Mult (Cos f) (ddx f)
ddx (Cos f) = Mult (Mult (Kons (-1)) (Sen f)) (ddx f)
ddx (Exp f) = Mult (Exp f) (ddx f)
ddx (Add f g) = Add (ddx f) (ddx g)

intnat :: Int -> Nat
intnat 0 = Z
intnat n = Suc (intnat (n-1))

printNat :: Nat -> IO ()
printNat = print . natToInt

printNatList :: [Nat] -> IO ()
printNatList = print . map natToInt

main :: IO ()
main = do
  printNatList $ fibonacciList (intnat 10)
