-- bibliotecas trabalho
-- system.console.ansi
-- system.io
-----------------------------------------------------------------------------------------------

--definir tuplas
--data TuplaDois a b = T2 a b

----------------------------------------------------------------------------------

-- Lista 12
--Ex1

data Lista a = Cons a (Lista a) | Vazio
	deriving(Eq, Show)

--lista [1,2,3,4]
ex1 :: Lista Int
ex1 = Cons 1(Cons 2 (Cons 3 (Cons 4 Vazio)))

ex2 :: Lista Int
ex2 = Cons 5(Cons 6 (Cons 7 (Cons 8 Vazio)))

tamanhoLis :: Lista a -> Int
tamanhoLis Vazio = 0
tamanhoLis (Cons x xs) = 1 + tamanhoLis xs
--para testar $ tamanhoLis ex1


--ex2
mapLis :: (a->b) -> Lista a -> Lista b
mapLis f Vazio = Vazio
mapLis f (Cons x xs) = Cons (f x) (mapLis f xs)   
--testar $ mapLis (+1) ex1

meuFoldinho :: (a-> b-> b) -> b -> Lista a -> b
meuFoldinho f i (Vazio) = i
meuFoldinho f i (Cons x xs) = (f x (meuFoldinho f i xs)) 
-- testar $ meuFoldinho (+) 2 ex1

meuFilterzin :: (a -> Bool) -> 	Lista a -> Lista a
meuFilterzin f Vazio       = Vazio
meuFilterzin f (Cons x xs)
	| f x = (Cons x (meuFilterzin f xs))
	| otherwise = meuFilterzin f xs

maisMais :: Lista a -> Lista a -> Lista a
maisMais Vazio Vazio 			 = Vazio
maisMais Vazio (Cons y ys)		 = (Cons y (maisMais (Vazio) (ys)))
maisMais (Cons x xs) (Cons y ys) = (Cons (x) (maisMais (xs) (Cons y ys)))
-- $ maisMais ex2 ex1

khalReverso :: Lista a -> Lista a
khalReverso (Cons x xs) =  