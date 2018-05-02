idade :: Int --valor inteiro constante
idade = 18 --nao eh variavel, sera 17 sempre

maiorDeIdade :: Bool 
maiorDeIdade = (idade>=18)

quadrado :: Int -> Int -- recebe inteiro e debolve um inteiro
quadrado x = x * x

mini :: Int -> Int -> Int -- recebe dois inteiros e retorna um inteiro
mini a b 
	| a <= b = a -- guarda(teste)  booleano = resposta
	|otherwise = b -- otherwise eh true


mi :: Int -> Bool
mi i = i >=18

igual :: Int -> Int -> Bool
igual a b = (a == b)

tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (y == z)