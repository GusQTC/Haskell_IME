import Control.Monad.Error.Class (Error)
import Distribution.Compat.Lens (_1)




----- 4.1

data Lista a = Nulo | a :>: (Lista a) deriving (Show)

removerElemento :: (Eq a) => a -> Lista a -> Lista a
removerElemento _ Nulo = Nulo
removerElemento x (y :>: ys) = if x == y then removerElemento x ys else y :>: removerElemento x ys

---------------- 4.2

data Paridade = Par | Impar deriving (Show)

decide :: Int -> Paridade
decide n = if n `mod` 2 == 0 then Par else Impar

list_decide :: [a] -> Paridade
list_decide [] = Par
list_decide (x : xs) = if length (x : xs) `mod` 2 == 0 then Par else Impar

bool_decide :: Bool -> Paridade
bool_decide False = Impar
bool_decide _ = Par

---------------------- 4.3

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show)

data Produto = Produto {valor_prod :: Double, tp :: TipoProduto} | Nada deriving (Show)

--------------------------- 4.4

data Arvore a = Galho a (Arvore a) (Arvore a) | Folha a | Null deriving Show