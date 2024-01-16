-- 2.1

data Mes
  = Janeiro
  | Fevereiro
  | Marco
  | Abril
  | Maio
  | Junho
  | Julho
  | Agosto
  | Setembro
  | Outubro
  | Novembro
  | Dezembro
  deriving (Show, Eq, Ord, Enum)

checaFim :: Mes -> Int
checaFim x = case x of
  Fevereiro -> 28
  Abril -> 30
  Junho -> 30
  Setembro -> 30
  Novembro -> 30
  _ -> 31

prox :: Mes -> Mes
prox = succ

estacao :: Mes -> String
estacao x = case x of
  x | x <= Marco -> "Verao"
  x | x >= Abril && x <= Junho -> "Outono"
  x | x >= Julho && x <= Setembro -> "Inverno"
  _ -> "Primavera" -- outubro, novembro, dezembro

-- 2.2

data Cripto
  = Mensagem String
  | Cifrado String
  | Erro String
  deriving (Show)

encriptar :: Cripto -> Cripto
encriptar (Mensagem x) = Cifrado ([succ y | y <- x])
encriptar (Cifrado x) = Erro "Mensagem ja cifrada"

desencriptar :: Cripto -> Cripto
desencriptar (Cifrado x) = Mensagem ([pred y | y <- x])
desencriptar (Mensagem x) = Erro "Mensagem nao cifrada"
