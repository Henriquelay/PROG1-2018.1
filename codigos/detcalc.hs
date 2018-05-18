{- CALCULADORA DE DETERMINANTE -}
det mtx i j = if length mtx /= length mtx!!0 /= length []
            then error"ENTRE COM UMA MATRIZ QUADRADA SEU IMBESIO ISSO NEM TEM DETER MINANTE"
            else show("Teste")
    where
        cofator = (-1)**(i+j)