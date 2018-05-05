{- script para determinar se um ponto pertence à área cinza -}
--assumindo que:    as figuras são círculos;
--                  os círculos todos têm o centro sobre a reta;
--                  os círculo todos são tangentes internos no ponto C;
--                  r é o raio do 1º círculo.
--c1, c2 = coords da origem, px, py, = coords do ponto, r = raio
areacinza cx cy px py r = ((pertaoC ax ay bx by (1.5*r)) && not(pertaoC ax ay bx by r)) || (pertaoC ax ay bx by (2.5*r)) && not (pertaoC ax ay bx by (2*r))
dist2pts ax ay bx by = sqrt((bx-ax)**2 + (by-ay)**2)
pertaoC ax ay bx by r = dist2pts ax ay bx by <= r