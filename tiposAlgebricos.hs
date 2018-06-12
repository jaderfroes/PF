--tipo algebrico = tipo q define atraves da enumeração dos elemetnos
data Temperatura = Frio | Calor   -- Temperatura pode assumir dois valores(como o Bool). Frio e Calor são dois construtores
	deriving(Eq,Show)     

data Estacao = Verao | Outono | Inverno | Primavera
	deriving(Eq,Show)

tempo :: Estacao  -> Temperatura
tempo Verao = Calor
tempo _ = Frio



--Exemplo 2
data Funcionario = Pessoa Nome Idade
	deriving(Eq, Show)

type Nome = String
type Idade = Int

andre :: Funcionario
andre = Pessoa "Andre Du Bois" 28

--Pessoa :: Nome -> Idade -> Funcionario



pegaNome :: Funcionario -> Nome
pegaNome (Pessoa n i) = n

pegaIdade :: Funcionario -> Idade
pegaIdade (Pessoa n i) = i


--Exemplo 3

data Forma = Circulo Float | Retangulo Float Float
	deriving(Eq, Show)

redondo :: Forma -> Bool
redondo (Circulo x ) =  True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r ) = pi * r * r
area (Retangulo b a) = b * a

--para testar
-- area (Retangulo 2.0 5.0)
-- area (Circulo 2.0)

--------------------------------------------------------------
--Exercicios


type Telefone = Int
type Endereco = String

data ItemDeLocadora  = CD String String | DVDS String String | Video String
	deriving(Eq, Show)

data SocioDeLocadora = Nome Telefone Endereco 
	deriving(Eq, Show)

type ItensDisponiveis = [ItemDeLocadora]
	

data ItemAlugado = ItemDeLocadora SocioDeLocadora 
	deriving(Eq, Show)


type ListaDeAlugados = [ItemAlugado]


alugaItem :: ItensDisponiveis -> ItemDeLocadora -> Bool
alugaItem [] _ = False
alugaItem (x:xs) i
    | x == i = True
    | otherwise = alugaItem xs i

-- como testar $ alugaItem [CD "Cazuza" "CD Bosta",CD "Legiao Urbana" "eh ruim"] (CD "Cazuza" "CD Bosta")

itensAlugadosSocio :: SocioDeLocadora -> ListaDeAlugados
itensAlugadosSocio s [] = []
itensAlugadosSocio s ((a,b):c)
	| s ==  b = s : itensAlugadosSocio s c
	| otherwise = itensAlugadosSocio s c 

