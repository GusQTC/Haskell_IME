-------3.1--------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Use even" #-}

filtro_par :: [Int] -> [Int]
filtro_par [] = []
filtro_par (x : xs) = if x `mod` 2 == 0 then x : filtro_par xs else filtro_par xs

filtro_impar :: [Int] -> [Int]
filtro_impar [] = []
filtro_impar (x : xs) = if x `mod` 2 /= 0 then x : filtro_impar xs else filtro_impar xs

-- 3.2 ----------------------------------------------

data Dinheiro = Dinheiro {valor :: Float, moeda :: String} deriving (Show)

dinheiroList :: [Dinheiro]
dinheiroList =
  [ Dinheiro {valor = 10.5, moeda = "USD"},
    Dinheiro {valor = 20.3, moeda = "BRL"},
    Dinheiro {valor = 15.2, moeda = "BRL"},
    Dinheiro {valor = 25.0, moeda = "USD"},
    Dinheiro {valor = 30.7, moeda = "USD"},
    Dinheiro {valor = 45.6, moeda = "BRL"},
    Dinheiro {valor = 50.0, moeda = "USD"},
    Dinheiro {valor = 60.8, moeda = "BRL"},
    Dinheiro {valor = 75.9, moeda = "BRL"},
    Dinheiro {valor = 80.0, moeda = "USD"}
  ]

convert_dol :: [Dinheiro] -> [Dinheiro]
convert_dol [] = []
convert_dol (Dinheiro {valor = x, moeda = "USD"} : xs) = Dinheiro {valor = x * 5.5, moeda = "BRL"} : convert_dol xs
convert_dol (Dinheiro {valor = x, moeda = "BRL"} : xs) = Dinheiro {valor = x, moeda = "BRL"} : convert_dol xs

convert_real :: [Dinheiro] -> [Dinheiro]
convert_real [] = []
convert_real (Dinheiro {valor = x, moeda = "BRL"} : xs) = Dinheiro {valor = x * 0.22, moeda = "USD"} : convert_real xs
convert_real (Dinheiro {valor = x, moeda = "USD"} : xs) = Dinheiro {valor = x, moeda = "USD"} : convert_real xs

filtrar_dolar :: [Dinheiro] -> [Dinheiro]
filtrar_dolar [] = []
filtrar_dolar (x : xs) = if moeda x == "USD" then x : filtrar_dolar xs else filtrar_dolar xs

somar_dolar :: [Dinheiro] -> Float
somar_dolar [] = 0
somar_dolar (x : xs) = sum [valor x | x <- filtrar_dolar (x : xs)]

aumentar_reais :: Float -> Dinheiro -> Dinheiro
aumentar_reais n Dinheiro {valor = x, moeda = "BRL"} = Dinheiro {valor = x + n, moeda = "BRL"}