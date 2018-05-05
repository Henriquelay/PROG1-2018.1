{- Definindo o tipo e o comportamento das minhas funções -}
type Ponto = (Float, Float)
first :: Ponto -> Float
second :: Ponto -> Float
pertaolosango :: Ponto -> Ponto -> Ponto -> Bool
{-                                                       -}

{-  Definindo funções seletoras -}
{- ---Módulos --- -}
first (x,y) = x
second (x,y) = y
maior a b   | a >= b = a
            | b > a = b
menor a b   | a <= b = a
            | b < a = b
{- checa se o valor x está no intervalo incluso de a e b -}
intervaloin x a b = (x <= maior a b) && (x >= menor a b)
intervaloex x a b = (x < maior a b) && (x > menor a b)  

{- angulo de uma reta sabendo dois pontos -}
anguloreta (x1,y1) (x2,y2) = (y2-y1)/(x2-x1)
ynox (x1,y1) (x2,y2) = (anguloreta (x1,y1) (x2,y2))*(x2-x1)+y1
{-                                                       -}

{- questão 1 I -}
pertaoretangulo (x,y) (dx,dy) (ex,ey) = intervaloin x dx ex && intervaloin y dy ey

{- questão 1 II -}
pertaolosango (x,y) (ex,ey) (sx,sy) = (y <= ynox (ex,ey) (sx,sy) && y >= ynox (ex,ey) (sx,(sy-2*ey)) && (y <= ynox ((ex+2*sx),ey) (sx,sy) && y >= ynox ((ex+2*sx),ey) (sx,(sy-2*ey)))