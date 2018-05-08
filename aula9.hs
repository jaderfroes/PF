--Polimorfismo de tipos

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

--LISTA 8
--EX2
concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x ++ concatena xs

--EX3
inverte :: [tal] -> [tal]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

--EX4
ultimo :: [x] -> x
ultimo [] = error "Error"
ultimo l = last(l)

inicio :: [x] -> [x]
inicio [] = []
inicio (x:xs)
	| tamanho(x:xs) == 1 = []
	| otherwise = x : inicio xs

-- EX5
take1 :: Int -> [l] -> [l]
take1 0 [] = []
take1 n (x:xs) = x : take (n-1) xs

drop1 :: Int -> [l] -> [l]
drop1 n [] = []
drop1 0 l = l
drop1 n (x:xs) = drop1 (n-1) xs
