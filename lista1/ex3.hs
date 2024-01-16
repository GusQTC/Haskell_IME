-------3.1--------------------------------------------

filtro_par :: [Int] -> [Int]
filtro_par [] = []
filtro_par (x : xs) = if x `mod` 2 == 0 then x : filtro_par xs else filtro_par xs

filtro_impar :: [Int] -> [Int]
filtro_impar [] = []
filtro_impar (x : xs) = if x `mod` 2 /= 0 then x : filtro_impar xs else filtro_impar xs

-- 3.2 ----------------------------------------------

data Dinheiro = Dinheiro {valor :: Float, moeda :: String} deriving (Show)

example = [Dinheiro {valor = 10.5, moeda = "USD"}, Dinheiro {valor = 20.3, moeda = "BRL"}, Dinheiro {valor = 15.2, moeda = "BRL"}]

real_para_dolar :: Dinheiro -> Dinheiro
real_para_dolar Dinheiro {valor = n, moeda = "BRL"} = Dinheiro {valor = (n * 0.2051), moeda = "USD"}

dolar_para_real :: Dinheiro -> Dinheiro
dolar_para_real Dinheiro {valor = n, moeda = "USD"} = Dinheiro {valor = (n * 4.87), moeda = ("BRL")}

filtrar_dolar :: [Dinheiro] -> [Dinheiro]
filtrar_dolar [] = []
filtrar_dolar (x : xs) = if moeda x == "USD" then x : filtrar_dolar xs else filtrar_dolar xs

somar_dolar :: [Dinheiro] -> Float
somar_dolar [] = 0
somar_dolar (x : xs) = sum [valor x | x <- filtrar_dolar (x : xs)]

aumentar_reais :: Float -> Dinheiro -> Dinheiro
aumentar_reais n Dinheiro {valor = x, moeda = "BRL"} = Dinheiro {valor = (x + n), moeda = "BRL"}