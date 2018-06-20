--arvore binaria

data Arvore a  = Folha a | No a (Arvore a) (Arvore a)
	deriving(Eq, Show)


arv1 :: Arvore Int
arv1 = No 3 (No 2 (Folha 4)(Folha 5)) (Folha 6)

arv2 :: Arvore Char
arv2 = No 'b' (Folha 'c') (No 'd' (Folha 'e') (No 'f' (Folha 'g')(Folha 'h'))) 



somaArvore :: Arvore Int -> Int
somaArvore (Folha x) = x
somaArvore (No v a1 a2) = v + somaArvore a1 + somaArvore a2
--lista 11
--1
multi2arv :: Arvore Int -> Arvore Int
multi2arv (Folha x)    = Folha (2*x)
multi2arv (No v a1 a2) = (No (2*v) (multi2arv a1) (multi2arv a2))

--2
contEl :: Arvore a -> Int
contEl (Folha x) = 1
contEl (No v a1 a2) = 1 + (contEl a1) + (contEl a2)

--3
maiorEl :: Arvore Int -> Int
maiorEl (Folha x) = x
maiorEl (No a a1 a2) = max (a) (max (maiorEl a1) (maiorEl a2))

--4
procuraInt :: Arvore Int -> Int -> Bool
procuraInt (Folha x) n = x == n
procuraInt (No x (a1) (a2)) n 
    | x == n = True
    | otherwise = procuraInt (a1) n || procuraInt (a2) n 

--5
contaInt :: Arvore Int -> Int -> Int
contaInt (Folha x) n
	|n == x    = 1
	|otherwise = 0
contaInt (No x (a1) (a2)) n
	|x == n = 1 + contaInt (a1) n + contaInt (a2) n  
	|otherwise = contaInt (a1) n + contaInt (a2) n

--6
refleteArv :: Arvore Int -> Arvore Int
refleteArv (Folha x) = (Folha x)
refleteArv (No x (a1) (a2)) = No x (a2) (a1)

--7
calcAltura :: Arvore Int -> Int
calcAltura (Folha x) = 1
calcAltura (No x (a1) (a2)) = calcAltura (a1) + calcAltura (a2)

--8
treeToList :: Arvore Int -> [Int]
treeToList (Folha x) = [x]
treeToList (No x (a1) (a2)) = x : treeToList a1 ++ treeToList a2

--9
mapTree :: (a -> b) -> Arvore a -> Arvore b
mapTree f (Folha x) = Folha (f(x))
mapTree f (No x (a1) (a2)) = (No (f(x)) (mapTree f (a1)) (mapTree f (a2)))
--para testar :   mapTree (+1) arv1

-----------------------------------------------------------------