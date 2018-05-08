{- Definindo o tipo e o comportamento das minhas funções -}
type Ponto = (Float, Float)
first :: Ponto -> Float
second :: Ponto -> Float
pertAoLosango :: Ponto -> Ponto -> Ponto -> Bool
{- ----------------------------------------------------- -}

{-  Definindo funções seletoras -}
{- ---Módulos --- -}
first (x,y) = x
second (x,y) = y
maior a b   | a >= b = a
            | b > a = b
menor a b   | a <= b = a
            | b < a = b
{- checa se o valor x está no intervalo incluso de a e b -}
intervaloIn x a b = (x <= maior a b) && (x >= menor a b)
intervaloEx x a b = (x < maior a b) && (x > menor a b)  
{- ----------------------------------------------------- -}

{- questão 1 I -}
pertAoRetangulo (x,y) (dx,dy) (ex,ey) = intervaloIn x dx ex && intervaloIn y dy ey

{- questão 1 II -}
pertAoLosango (x,y) (ex,ey) (sx,sy) = (entreAsRetas (x,y) (ex,ey) (sx,sy) (ex,ey) (sx,sy-2*ey)) && (entreAsRetas (x,y) (ex+2*sx,ey) (sx,sy) (ex+2*sx,ey) (sx,sy-2*ey))

{- ---------------------------------------------------- -}
{- Definindo formulas gerais de retas dados dois pontos -}
coefAngular (x1,y1) (x2,y2) = (y1-y2)/(x1-x2)
coefLinear (x1,y1) (x2,y2) = y1 - coefAngular (x1,y1) (x2,y2) * x1
imagemNoX xp (x1,y1) (x2,y2) = coefAngular (x1,y1) (x2,y2) * xp + coefLinear (x1,y1) (x2,y2)
{- Definindo relações de um ponto X para uma reta genérica -}
acimaDaReta (x,y) (xp1,yp1) (xp2,yp2) = y >= imagemNoX x (xp1,yp1) (xp2,yp2)
abaixoDaReta (x,y) (xp1,yp1) (xp2,yp2) = y <= imagemNoX x (xp1,yp1) (xp2,yp2)
{- Note que xP1 é a reta maior e portanto P está em maiúsculo -}
entreAsRetas (x,y) (xP1,yP1) (xP2,yP2) (xp3,yp3) (xp4,yp4) = abaixoDaReta (x,y) (xP1,yP1) (xP2,yP2) && acimaDaReta (x,y) (xp3,yp3) (xp4,yp4)
{- ---------------------------------------------------- -}

{- questão 1 III -}
pertAoCirculo (x,y) (cx,cy) r = if distP (x,y) (cx,cy) <= r
                                then "Pertence ao circulo"
                                else "Nao pertence ao circulo"

{- distancia entre dois pontos qualquer -}
distP (x1,y1) (x2,y2) = sqrt((x1-x2)**2 + (y1-y2)**2 )  