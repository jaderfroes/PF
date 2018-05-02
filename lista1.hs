--https://sites.google.com/site/semanticaufpel/

-- Ex1
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a == b) && (b == c) && (c == d) 

-- Ex2
{-quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c 
	|(a == b) && (b == c) = 3
	|((a == b) && (b /= c)) || ((a == c) && (b /= c)) || ((b == c) && (b /= a)) = 2
	|otherwise = 0-}

-- Ex3
todosSaoDiferentes :: Int -> Int -> Int -> Bool
todosSaoDiferentes a b c = (a /= b) && (b /= c) && (c /= a)

--Ex 6
todoSaoIguais :: Int -> Int -> Int -> Bool
todoSaoIguais x y z = (x == y) && (y == z)

-- Ex7
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
 |todosSaoDiferentes a b c = 0
 |todoSaoIguais a b c = 3
 |otherwise = 2

-- Ex8
elevadoDois :: Int -> Int
elevadoDois n = n * n

-- Ex9
elevadoQuatro :: Int -> Int
elevadoQuatro n = elevadoDois n * elevadoDois n

-- Ex10
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 13
vendas 2 = 9
vendas n = 20

vendasTotal :: Int -> Int
vendasTotal 0 = vendas 0
vendasTotal n = vendasTotal(n-1) + vendas(n) 