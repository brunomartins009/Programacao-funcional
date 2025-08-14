--define o periodo de recursão
periodo :: Int
periodo = 7

--tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

--função que inverte lista
reverte :: [Int] -> [Int]
reverte [] = []
reverte (a:x) = reverte (x) ++ [a]

--função que retorna uma lista de vendas
listaVendas :: Int -> [Int]
listaVendas 0 = [vendas 0]
listaVendas x = listaVendas (x-1) ++ [vendas x]

--função que retorna uma lista com apenas numeros inteiros

listaImpar :: [Int] -> [Int]
listaImpar [] = []
listaImpar (a:x)
	|mod a 2 /= 0 = [a] ++ listaImpar x
	|otherwise = listaImpar x
	
--função que retorna uma lista de lista com dia e venda
listaVendasC :: Int -> [[Int]]
listaVendasC 0 = [[0, vendas 0]]
listaVendasC x = listaVendasC (x-1) ++ [[x, vendas x]]

--função que ordena uma lista de inteiros
{-ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (a:x)
	|a > ordenaLista x
-}	
--função que insere um elemento em uma lista ordenada
insere :: Int -> [Int] -> [Int]
insere y [] = [y]
insere y (a:x)
	|y < a = y:a:x
	|otherwise = [a] ++ insere y x
	
--função que ordena lista de lista pelo primeiro elemento

--função que insere lista de inteiro um lista de lista ordenada
insereLista :: [Int] -> [[Int]] -> [[Int]]
insereLista y [] = [y]
insereLista y (a:x)
	|y < a = y:a:x
	|otherwise = [] ++ insereLista y x
