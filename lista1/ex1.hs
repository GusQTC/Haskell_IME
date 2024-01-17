------  1,1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use even" #-}
list_a = [x | x <- [0, 1 .. 100], x < 51 && x `mod` 5 == 0]

vowels = ['a', 'e', 'i', 'o', 'u']

list_b = [x | x <- ['a' .. 'z'], x `notElem` vowels]

restrict = [2, 7, 13, 35, 42]

list_c = [x | x <- [0, 1 .. 100], x `notElem` restrict, x < 51]

list_d = [(x, y) | x <- [0, 1 .. 8], y <- ['a' .. 'h']]

-- 1.2

string_par :: String -> Bool
string_par [] = True
string_par [x] = False
string_par (x : xs) = if length (x : xs) `mod` 2 == 0 then True else False

string_vetor :: String -> [Char]
string_vetor [] = []
string_vetor (x : xs) = reverse (x : xs)

head :: [a] -> a   -- NEEDS TO COMPOSE ANOTHER FUNCTION
head [] = error "Lista vazia"
head (x : xs) = x

int_binario :: Int -> [Int]
int_binario 0 = [0]
int_binario 1 = [1]
int_binario n = int_binario (n `div` 2) ++ [n `mod` 2]
