--EX 1
membro :: [Int] -> Int -> Bool
membro [] _ = False
membro (x:xs) n
    |n == x = True
    |otherwise = membro xs n

--Ex2
membroNum :: [Int] -> Int -> Int
membroNum [] _ = 0
membroNum (x:xs) n
    |x == n    = 1 + membroNum xs n
    |otherwise = membroNum xs n

--Ex3
membro2 :: [Int] -> Int -> Bool
membro2 [] _ = False
membro2 l n 
    |membroNum l n /= 0 = True
    |otherwise = False

--Ex4
unico :: [Int] -> [Int]
unico l = unico2 l l

unico2 :: [Int] -> [Int] -> [Int]
unico2 [] _ = []
unico2 (x:xs) l
	|membroNum l x == 1 = x : unico2 xs l
	|otherwise 	  		 = unico2 xs l

--Ex5
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins a (x:xs)
    | a <= x = (a:x:xs) 
    |otherwise = x: ins a (xs)

membroOrder :: [Int] -> Int -> Bool
membroOrder lis n = membroOrder2 (iSort lis) n

membroOrder2 :: [Int] -> Int -> Bool
membroOrder2 (x:xs) n 
	| n > x     = False
	| n == x    = True
	| otherwise = membroOrder2 xs n 