{- verificando se um numero x pertence ao intervalo (0,100) e é divisível por 3 e 5 -}
intervalo0100ediv3e5 x = divisivel3e5 (x) && maiorque0emenorque100 (x)
divisivel3 x = (mod x 3) == 0
divisivel5 x = (mod x 5) == 0
divisivel3e5 x = divisivel3 (x) && divisivel5 (x)
maiorque0 x = x > 0
menorque100 x = x < 100
maiorque0emenorque100 x = maiorque0 (x) && menorque100 (x)
{- ou exclusivo -}
oux a b = if a == True && b == True                      
         then False
         else a || b
{- verificando quantos dias faltam do projeto -}
diasprojeto dia1 mes1 ano1 dia2 mes2 ano2  = if (diferencadata dia2 mes2 ano2 dia1 mes1 ano1) < 0
                                             then 0
                                             else diferencadata dia2 mes2 ano2 dia1 mes1 ano1
diasatemes mes = mes * 30
diasateano ano = ano * 365
diasatedata dia mes ano = dia + (diasatemes (mes)) + (diasateano (ano))
diferencadata dia1 mes1 ano1 dia2 mes2 ano2 = diasatedata dia2 mes2 ano2 - diasatedata dia1 mes1 ano1
{- verificando se 3 aretas, A, B e C podem formar um triângulo -}
formatriangulo a b c = a < (b + c) || b < (a + c) || c < (a + b)
{- questoes de pertinencia de um ponto a uma regiao no plano -}
{- pertence a um retangulo parelelo aos eixos X e Y -}
pertenceaoretparal ex ey dx dy px py = if ( dx <= px && px <= ex ) && ( ey <= py && ey <= dy ) --verificando se px está entre (ou igual) dx e ex. o mesmo vale pra py--
                                       then putStrLn "Pertence ao retangulo"
                                       else putStrLn "Nao pertence"
{- pertence ao losango de eixos paralelos aos eixos X e Y -}
pertenceaolosango ez ey sx sy px py = if (px >= sx)
                                      then if (py >= ey)
                                           then if (px <= ax+sy)