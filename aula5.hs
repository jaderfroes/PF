tamanho :: [a] -> Int
tamanho[] = 0
tamanho (x:xs) = 1 + tamanho xs

-- Ordenação Insertion

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins a (x:xs)
    | a <= x = (a:x:xs) 
    |otherwise = x: ins a (xs)

-- Ordenação Quick
qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = qs (menores x xs)
			++ [x] ++
			qs (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores a [] = []
menores a (x:xs)
    | x <= a = x: menores a (xs)
   	| otherwise = menores a (xs) 

maiores :: Int -> [Int] -> [Int]
maiores a [] = []
maiores a (x:xs)
	| x >= a = x: maiores a (xs)
	| otherwise = maiores a (xs)
-- Modificar o iSort para eliminar repetições
iSort2 :: [Int] -> [Int]
iSort2 [] = []
iSort2 (x:xs) = ins2 x (iSort2 xs)

ins2 :: Int -> [Int] -> [Int]
ins2 x [] = [x]
ins2 a (x:xs)
    | a < x = (a:x:xs)
    | a == x = x:xs
    | otherwise = x: ins2 a (xs)

-- Modificar o iSort para ordenar de forma decrescente
iSort3 :: [Int] -> [Int]
iSort3 [] = []
iSort3 (x:xs) = ins3 x (iSort3 xs)

ins3 :: Int -> [Int] -> [Int]
ins3 x [] = [x]
ins3 a (x:xs)
	| a > x = (a:x:xs)
	| otherwise = x: ins3 a (xs)

-- Entra uma lista e retorna o menor e o maior
minEmax :: [Int] -> (Int, Int)
minEmax [] = (0,0)
minEmax (x) = (menor (iSort x), maior(iSort3 x)) 

menor :: [Int] -> Int
menor [] = 0
menor (x:xs) = x

maior :: [Int] -> Int
maior [] = 0
maior (x:xs) = x


--Fazer lista 5


--uso do let
-- let l = ?
-- in




