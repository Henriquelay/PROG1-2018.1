{- Definindo o tipo e o comportamento das minhas funções -}
type Ponto = (Float, Float)
{- ----------------------------------------------------- -}

{- questão 1 I -}
pertAoRetangulo (x,y) (dx,dy) (ex,ey) = intervaloIn x dx ex && intervaloIn y dy ey

{- checa se o valor x está no intervalo incluso de a e b -}
intervaloIn x a b = (x <= maior a b) && (x >= menor a b)
--intervaloEx x a b = (x < maior a b) && (x > menor a b)
maior a b   
        | a >= b = a
        | b > a = b
menor a b
        | a <= b = a
        | b < a = b
{- ----------------------------------------------------- -}

{- questão 1 II -}
pertAoLosango (x,y) (ex,ey) (sx,sy) = (entreAsRetas (x,y) (ex,ey) (sx,sy) (ex,ey) (sx,sy-2*ey)) && (entreAsRetas (x,y) (ex+2*sx,ey) (sx,sy) (ex+2*sx,ey) (sx,sy-2*ey))
                                        where {- Definindo formulas gerais de retas dados dois pontos -}
                                                coefAngular (x1,y1) (x2,y2) = (y1-y2)/(x1-x2)
                                                coefLinear (x1,y1) (x2,y2) = y1 - coefAngular (x1,y1) (x2,y2) * x1
                                                imagemNoX xp (x1,y1) (x2,y2) = coefAngular (x1,y1) (x2,y2) * xp + coefLinear (x1,y1) (x2,y2)
                                                {- Definindo relações de um ponto X para uma reta genérica -}
                                                acimaDaReta (x,y) (xp1,yp1) (xp2,yp2) = y >= imagemNoX x (xp1,yp1) (xp2,yp2)
                                                abaixoDaReta (x,y) (xp1,yp1) (xp2,yp2) = y <= imagemNoX x (xp1,yp1) (xp2,yp2)
                                                {- Note que xP1 é a reta maior e portanto P está em maiúsculo -}
                                                entreAsRetas (x,y) (xP1,yP1) (xP2,yP2) (xp3,yp3) (xp4,yp4) = abaixoDaReta (x,y) (xP1,yP1) (xP2,yP2) && acimaDaReta (x,y) (xp3,yp3) (xp4,yp4)

{- questão 1 III -}
pertAoCirculo (x,y) (cx,cy) r   
                            | distP (x,y) (cx,cy) <= r = "Pertence ao circulo"
                            | otherwise = "Nao pertence ao circulo"

{- distancia entre dois pontos qualquer -}
distP (x1,y1) (x2,y2) = sqrt((x1-x2)**2 + (y1-y2)**2 )
{- --------------------------------------------------- -}

{- Questão 2 -}
dist3P2a2 (x1,y1) (x2,y2) (x3,y3) = "Ponto 1 e 2 = " ++ show (distP (x1,y1) (x2,y2)) ++ "; " ++
                                    "Ponto 1 e 3 = " ++ show (distP (x1,y1) (x3,y3)) ++ "; " ++
                                    "Ponto 2 e 3 = " ++ show (distP (x2,y2) (x3,y3))

{- Questão 3 -}
eq2Grau a b c = if delta < 0
                then error "O delta dessa equação é menor que 0!! Pelo menos uma raíz dela não pertence conjundo Real"
                else if a == 0
                    then error "Não me faça dividir por zero seu sacana! >:( Quer destruir o universo?!"
                    else show ((baskharaPos,baskharaNeg))
                        where
                        {- Delta e baskhara da eq quadratica -}
                                delta = b**b - (4*a*c)
                                baskharaPos = (-b+sqrt(delta))/(2*a)
                                baskharaNeg = (-b-sqrt(delta))/(2*a)

{- Questão 4 I -}
relacao a b c
                | a /= b && a /= c && b /= c = show ("Os tres numero sao diferentes entre si.")
                | (a == b && a /= c)||(a /= b && a == c)||(b == c && b /= a) = show ("Apenas dois numeros sao iguais.")
                | a == b && a == b = show ("Os tres numeros sao iguais")

{- Questão 4 II -}
quantasQuadRetangulo (sex,sey) (idx,idy) =     if primeiroQuad (sex,sey)
                                                then if quartQuad (idx,idy)
                                                        then show ("Ocupa 2 quadrantes")
                                                        else show ("Ocupa 1 quadrante")
                                                else if terceiroQuad (sex,sey)
                                                        then if quartQuad (idx,idy)
                                                                then show ("Ocupa 2 quadrantes")
                                                                else show ("Ocupa 1 quadrante")
                                                        else if segundoQuad (sex,sey)
                                                                then if primeiroQuad (idx,idy)
                                                                        then show ("Ocupa 2 quadrantes")
                                                                        else if terceiroQuad (idx,idy)
                                                                                then show ("Ocupa 2 quadrantes")
                                                                                else if quartQuad (idx,idy)
                                                                                        then show ("Ocupa 4 quadrantes")
                                                                                        else show ("Ocupa 1 quadrante")
                                                                else show ("Ocupa 1 quadrante")

                                                where {- Pertinencias a quadrantes -}
                                                        primeiroQuad (x,y) = x > 0 && y > 0
                                                        segundoQuad (x,y) = x < 0 && y > 0
                                                        terceiroQuad (x,y) = x < 0 && y < 0
                                                        quartQuad (x,y) = x > 0 && y < 0

{- Questão 5 -}
calcImc kg m 
        | imc >= 30 = "Obeso"
        | imc >= 25 && imc < 30 = "Acima"
        | imc > 18.5 && imc < 25 = "Normal"
        | imc <= 18.5 = "Baixo"
                where imc = kg / (m)**2

{- Questão 6 -}
descontoNoIR reais
                | reais < 500 = show ("0%, você é isento.")
                | reais >= 500 && reais < 1500 = show ("10% de desconto no IR")
                | reais >= 1500 && reais < 2500 = show ("15% de desconto no IR")
                | reais >= 2500 = show("25% de desconto no IR")

{- Questão 7 -}
tipoTriang :: Ponto -> Ponto -> Ponto -> String
tipoTriang (x1,y1) (x2,y2) (x3,y3) = if aresta1 == aresta2 && aresta1 == aresta3
                                        then "O triangulo e equilatro"
                                        else if aresta1 == aresta2 || aresta1 == aresta3 || aresta2 == aresta3
                                                then if hipo aresta1 aresta2 == aresta3 || hipo aresta1 aresta3 == aresta2 || hipo aresta2 aresta3 == aresta1
                                                        then "O triangulo e retangulo e isosceles"
                                                        else "O triangulo e isosceles"
                                                else if hipo aresta1 aresta2 == aresta3 || hipo aresta1 aresta3 == aresta2 || hipo aresta2 aresta3 == aresta1
                                                        then "O triangulo e retangulo"
                                                        else "O triangulo nao tem relacao"

                                        where  
                                        hipo x y = sqrt(x**2+y**2)
                                        aresta1 = distP (x1,y1) (x2,y2)
                                        aresta2 = distP (x1,y1) (x3,y3)
                                        aresta3 = distP (x2,y2) (x3,y3)

{- Questão 8 -}
type Data = (Int,Int,Int)
idade :: Data -> Data -> Int
idade nasc atual = if second nasc > second atual
                   then third atual - third nasc - 1
                   else if first nasc > first atual
                        then third atual - third nasc - 1
                        else third atual - third nasc

{- Definindo funções seletoras -}
first (x,y,z) = x
second (x,y,z) = y
third (x,y,z) = z

{- Questão 9 -}
type PontoInt = (Int,Int)
movBispo :: PontoInt -> Char
movBispo (x,y)
        | x > 8 || y > 8 = '0'
        | x == 8 = 'E'
        | x == 1 = 'D'
        | otherwise = {- 'D' ++ 'E' -} error "O bispo se move em mais de uma direção. Não posso responder usando o tipo Char usando somente 'D' e 'E'."

{- Questão 10 -}
idadePlaneta :: Float -> String -> Float
idadePlaneta seg planeta
                        | planeta == "Terra" = idadeTerra
                        | planeta == "Mercúrio" = idadeMercurio
                        | planeta == "Vênus" = idadeVenus
                        | planeta == "Marte" = idadeMarte
                        | planeta == "Júpiter" = idadeJupiter
                        | planeta == "Saturno" = idadeSaturno
                        | planeta == "Urano" = idadeUrano
                        | planeta == "Netuno" = idadeNetuno

                        where
                        idadeTerra = seg / 31557600                              -- = (365.25*24*60*60)
                        idadeMercurio = idadeTerra * 2408467 * (10**(-7))      -- = 0.2408467 
                        idadeVenus = idadeTerra * 61519726 * (10**(-8))        -- = 0.61519726
                        idadeMarte = idadeTerra * 18808158 * (10**(-7))         -- = 1.8808158 
                        idadeJupiter = idadeTerra * 11862615 * (10**(-6))       -- = 11.862615  
                        idadeSaturno = idadeTerra * 29447498 * (10**(-6))       -- = 29.447498  
                        idadeUrano = idadeTerra * 84016846 * (10**(-6))         -- = 84.016846
                        idadeNetuno = idadeTerra * 16479132 * (10**(-5))         -- = 164.79132