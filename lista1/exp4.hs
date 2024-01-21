{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use even" #-}

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

{-
Crie o tipo TipoProduto que possui os values constructors Escritorio , Informatica , Livro , Filme e Total .
O tipo Produto possui um value constructor - de mesmo nome - e os campos valor ( Double ), tp ( TipoProduto
) e um value constructor Nada , que representa a ausˆencia de um Produto .

Deseja-se calcular o valor total de uma compra, de modo a n˜ao ter nenhuma convers˜ao para inteiro e de forma
combin´avel. Crie uma instˆancia de semigrupo e monoide para Produto , de modo que o retorno sempre tenha
Total no campo tp e a soma dos dois produtos m valor . Explique como seria o exerc´ıcio sem o uso de monoides.
Qual(is) seria(m) a(s) diferen¸ca(s)?
-}

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show)

data Produto = Produto {valor_prod :: Double, tp :: TipoProduto} | Nada deriving (Show)

class Semigrupo a where
  totais :: a -> Produto -> Produto

instance Semigrupo Produto where
  totais (Produto a _) (Produto b _) = Produto (a + b) Total
  totais (Produto a _) Nada = Produto a Total
  totais Nada (Produto b _) = Produto b Total
  totais Nada Nada = Nada

-- example produto
produtos = [Produto 10.0 Livro, Produto 20.0 Escritorio, Produto 30.0 Filme, Produto 40.0 Informatica]

{--
Sem o uso de monoides, a lógica de combinação de produtos teria que ser tratada manualmente, o que geralmente resultaria em código mais extenso e menos expressivo. No caso específico do cálculo do valor total de uma compra, a abordagem sem monoides envolveria a verificação explícita de Nada e a soma dos valores. Sem o uso de monoides, a lógica de combinação fica mais explícita, e você precisa criar funções específicas para a combinação e identidade, que são automaticamente tratadas pelas instâncias de monoides. O uso de monoides simplifica o código, tornando-o mais conciso e abstrato, facilitando a composição de valores e reduzindo a necessidade de tratamento manual de casos especiais.
-}
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