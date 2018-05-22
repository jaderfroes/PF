--lista8
inverte :: [Int] -> [Int]
inverte l = foldr (insFin) [] l

insFin :: Int -> [Int] -> [Int]
insFin n l = l ++ [n]

--ex4
ultimo :: [a] -> a -- retorna o ́ultimo elemento de uma lista
ultimo l = foldr (last) [] l