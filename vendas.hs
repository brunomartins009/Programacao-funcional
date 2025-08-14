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


-- função que retorna o total de vendas

totalVendas :: Int -> Int
totalVendas 0 = 0
totalVendas x = vendas x + totalVendas(x-1)

-- função que soma todas as vendas do periodo para venda par

totalVendasPar :: Int -> Int
totalVendasPar 0 = 0
totalVendasPar d
	|mod (vendas d) 2 == 0 = vendas d + totalVendasPar(d-1)
	|otherwise = totalVendasPar (d-1)

-- função que retorna quantas vendas superam um valor
maxVenda :: Int -> Int -> Int
maxVenda 0 _ = 0
maxVenda d v
	|vendas d > v = 1 + maxVenda(d-1) v
	|otherwise = maxVenda (d-1) v
 
maxVnd :: Int -> Int
maxVnd x = maxVenda periodo x

-- retorna maior venda, mas depende do parâmetro

maiorVenda :: Int -> Int
maiorVenda 0 = 0
maiorVenda d = maior (vendas d) (maiorVenda (d-1))

maior :: Int -> Int -> Int
maior a b
	|a > b = a
	|otherwise = b

-- retorna maior venda

-- retorna o dia de certa venda, mas depende do parâmetro

diaVenda :: Int -> Int -> Int
diaVenda 0 _ = -1
diaVenda d v
	|vendas d == v = d
	|otherwise = diaVenda (d-1) v

-- dia de certa venda
diaVendaS :: Int -> Int
diaVendaS v = diaVenda periodo v