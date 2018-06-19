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
multi2arv :: Arvore Int -> Arvore Int
multi2arv (Folha x)    = Folha (2*x)
multi2arv (No v a1 a2) = (No (2*v) (multi2arv a1) (multi2arv a2))

contEl :: Arvore a -> Int
contEl (Folha x) = 1
contEl (No v a1 a2) = 1 + (contEl a1) + (contEl a2)

maiorEl :: Arvore Int -> Int
maiorEl (Folha x) = x
maiorEl (No a a1 a2) = max (a) (max (maiorEl a1) (maiorEl a2)) 