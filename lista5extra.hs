--1

retornaUltimo :: [a] -> a
retornaUltimo [] = error "não existe ultimo, otário"
retornaUltimo (x) = head(reverse(x))

--2
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao _ [] = 0
pegaPosicao n (x:xs)
	| n == 1 = head(x:xs)
    | otherwise = pegaPosicao (n-1) (xs)

--3
pega :: Int -> [Int] -> [Int]
pega _ [] = []
pega 1 x = [head x]
pega n (x:xs) = x : pega (n-1) xs


--4
retira :: Int -> [a] -> [a]
retira _ [] = error "não existe elemento pra retirar"
retira 1 (x:xs) = xs
retira n (x:xs) = retira (n-1) xs

--5
 