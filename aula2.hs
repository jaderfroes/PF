{-recursao com inteiros-}
--soma dos naturais de 0 ate n
somaNaturais :: Int -> Int
somaNaturais 0 = 0
somaNaturais n = n + somaNaturais (n-1)

-- Lista de exercicios 2

--EX1
maximo :: Int -> Int -> Int
maximo a b
 | a > b = a
 | otherwise = b

--EX2
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 13
vendas 2 = 0
vendas _ = 200


maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maximo (vendas n) (maiorVenda (n-1))

--Ex3
maxVenda :: Int -> Int
maxVenda 0 = 0
maxVenda n 
    | maiorVenda n == vendas n = n
    | otherwise  = maxVenda (n-1)

-- Ex4
zeroVendas :: Int -> Int
zeroVendas n
    | vendas n == 0 = n
    | n >= 0 = zeroVendas(n-1)
    | otherwise = -1


--Ex5
valorIgual :: Int-> Int -> Int
valorIgual s n
    | s == vendas n = n
    | n>=0          = valorIgual s (n-1)
    |otherwise = -1

--Ex6
{-enviando o s como 0-}

--Ex7
maxVenda2 :: Int -> Int -> Int
maxVenda2 m n 
    | m == n