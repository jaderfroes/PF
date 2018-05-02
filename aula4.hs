type Pessoa = (String, String, Int)

joao :: Pessoa
joao = ("Joao Silva", "222-2222", 17)

nome :: Pessoa -> String
nome(n, t, i) = n 

telefone :: Pessoa -> String
telefone(n, t, i ) = t

idade :: Pessoa -> Int
idade (n, t, i) = i


--Ex1

adicionaTupla :: (Int, Int) -> Int
adicionaTupla (a,b) = a + b

--Ex2
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift (a, b) = (b, a)

--Ex3


--Extra
venda :: Int->Int
venda 0 = 2
venda 1 = 3
venda 2 = 4
venda 3 = 2

vendaTotal :: Int -> Int
vendaTotal 0 = venda 0
vendaTotal n = (venda n) + (vendaTotal $ n-1)

tabela :: Int -> String 
tabela n = cabeca ++ geraVendas n ++ geraTotal n 

cabeca :: String
cabeca = "Semana\t      Venda\n"

geraVendas :: Int -> String
geraVendas 0 = geraVenda 0
geraVendas n = geraVendas(n-1) ++ geraVenda(n)

geraVenda :: Int-> String
geraVenda n = "Semana " ++ show n ++"\t" ++  show (venda n) ++ "\n"

geraTotal :: Int -> String
geraTotal n = "Total\t        " ++ show  (vendaTotal n) ++"\n"