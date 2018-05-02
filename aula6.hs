somaPares :: [(Int,Int)] -> Int
somaPares [] = 0
somaPares (a:x) = fst a + snd a + somaPares x

--LISTA 6 Ex1
somaTripla :: [(Int, Int, Int)] -> Int
somaTripla [] = 0
somaTripla ((a,b,c):x) = a + b + c + somaTripla x

--Ex2
somaTuplas :: [((Int,Int),(Int,Int))] -> Int
somaTuplas [] = 0
somaTuplas ((a,b):x) = somaPares[a] + somaPares[b] + somaTuplas x -- a e b são tuplas com 2 elementos

--Ex3
zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp [] _ = []
zipp _ [] = []
zipp (a:b) (x:xs) = [(a,x)] ++ zipp b xs

--Ex4
zipp3 ::[Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
zipp3 [] _ _ = []
zipp3 _ [] _ = []
zipp3 _ _ [] = []
zipp3 (a:b) (c:d) (e:f) = [(a,c,e)] ++ zipp3 b d f


--Ex5
unZipp :: [(Int,Int)] -> ([Int], [Int])
unZipp l = (unZipEsq l, unZipDir l)

unZipEsq :: [(Int, Int)] -> [Int]
unZipEsq [] = []
unZipEsq (x:xs) = fst x : unZipEsq xs  

unZipDir :: [(Int, Int)] -> [Int]
unZipDir [] = []
unZipDir (x:xs) = snd x : unZipDir xs