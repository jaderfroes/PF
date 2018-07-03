ex = [1,2,3,4]
-- [x|x <- ex, mod x 2 == 0] retorna pares
--[2*x | x <- ex] retorna lista com valor dobrado
--[(a,b) | a <- [1,2,3,4], b <- [5,6,7,8]]



somaTuplas :: [(Int, Int)] -> [Int]
somaTuplas l = [a+b | (a,b) <- l]

addOrdPairs :: [(Int, Int)] -> [Int]
addOrdPairs l = [ a+b | (a,b) <- l, a<b]

----------------------------------------------------------------------------------
--LISTA 13

--Ex1

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter f l = [x |x <-l, f x] --um elemento x(retirado de l) eh inserido se f aplicado a x for true

--Ex2
mapZin :: (a->b) -> [a] -> [b]
mapZin f l = [f x | x <- l]

--Ex3
removeEspacos :: String -> String
removeEspacos s = [x |x <- s, x /= ' ']

--Ex4
sings :: [[a]] -> [a]
sings l = [x | x:xs <-l,length(x:xs) == 1] 

--Ex5
matches :: Int -> [Int] -> [Int]
matches n l = [x |  x<- l,  x == n]

--Ex6
elemento :: Int -> [Int] -> Bool
elemento n l = length(matches n l) /= 0 

