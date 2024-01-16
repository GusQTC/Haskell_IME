import Control.Monad.Error.Class (Error)
import Distribution.Compat.Lens (_1)
import Distribution.Simple.Test (test)

----- 4.1

data Lista a = Nulo | a :>: (Lista a) deriving (Show)

removerElemento :: (Eq a) => a -> Lista a -> Lista a
removerElemento _ Nulo = Nulo
removerElemento x (y :>: ys) = if x == y then removerElemento x ys else y :>: removerElemento x ys

exemple :: Lista Int
exemple = 1 :>: (2 :>: (3 :>: Nulo))

---------------- 4.2

data Paridade = Par | Impar deriving (Show)

class ParImpar a where
  decide :: a -> Paridade

instance ParImpar Int where
  decide n = if n `mod` 2 == 0 then Par else Impar

instance ParImpar [a] where
  decide xs = if length xs `mod` 2 == 0 then Par else Impar

instance ParImpar Bool where
  decide False = Par
  decide True = Impar

---------------------- 4.3

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show)

data Produto = Produto {valor_prod :: Double, tp :: TipoProduto} | Nada deriving (Show)

--------------------------- 4.4

data Arvore a = Galho a (Arvore a) (Arvore a) | Folha a | Null deriving (Show)

preOrder :: Arvore a -> [a]
preOrder Null = []
preOrder (Folha a) = [a]
preOrder (Galho a left right) = [a] ++ preOrder left ++ preOrder right

inOrder :: Arvore a -> [a]
inOrder Null = []
inOrder (Folha a) = [a]
inOrder (Galho a left right) = inOrder left ++ [a] ++ inOrder right

teste :: Arvore Int
teste = Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Null)) (Galho 20 Null (Galho 22 (Folha 21) Null))

treeSum :: Arvore Int -> Int
treeSum Null = 0
treeSum (Folha a) = a
treeSum (Galho a left right) = a + treeSum left + treeSum right