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
prox x = if x == Dezembro then Janeiro else succ x

data Hemisferio = Norte | Sul
    deriving (Show, Eq)

estacao :: Mes -> Hemisferio -> String
estacao mes hemisferio
    | hemisferio == Norte = case mes of
        Janeiro -> "Inverno"
        Fevereiro -> "Inverno"
        Marco -> "Primavera"
        Abril -> "Primavera"
        Maio -> "Primavera"
        Junho -> "Verao"
        Julho -> "Verao"
        Agosto -> "Verao"
        Setembro -> "Outono"
        Outubro -> "Outono"
        Novembro -> "Outono"
        Dezembro -> "Inverno"

    | hemisferio == Sul = case mes of
        Janeiro -> "Verao"
        Fevereiro -> "Verao"
        Marco -> "Outono"
        Abril -> "Outono"
        Maio -> "Outono"
        Junho -> "Inverno"
        Julho -> "Inverno"
        Agosto -> "Inverno"
        Setembro -> "Primavera"
        Outubro -> "Primavera"
        Novembro -> "Primavera"
        Dezembro -> "Verao"

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
