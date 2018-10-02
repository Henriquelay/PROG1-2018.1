-- a) banco imobiliário
pagamento valor = ( div ( valor ) 500 ) + (div ( mod ( valor ) 500 ) 100 ) + (div ( mod ( valor ) 100 ) 50 ) + (div ( mod ( valor ) 50 ) 10 ) + ( mod ( valor ) 5 )
-- b) area de um retangulo
arearetangulo a b = ( a ) * ( b )
-- c) area de um circulo
areacirculo r = pi * ( r ) ** 2
-- d) distacia entre dois pontos no plano
dist x1 y1 x2 y2 = sqrt ( ( x2 - x1 ) ** 2 + ( y2 - y1 ) ** 2 )
-- e) raizes de uma equacao de segundo grau (incompleta)
delta a b c = b ** 2 - 4 * a * c
-- raizes a b c = 
-- f) potecia de um numero elevado a expoente negativo
potneg a b = 1 /  ( ( a ) ^ ( b ) )
-- g) temperatura de celsius para fahrenheit
cparaf c = ( c ) * 9 / 5 + 32
-- h) ganho anual de um valor aplicado em um banco por 12 meses, na taxa de 0,5 ao mes
jurossimp valor = ( valor ) * ( taxa ) ( 0.5 ) * 12
taxa i = ( i ) / 100
-- i) aureola exterior do circulo conforme figura no pdf.
areaaureola c = areacirculo ( 1.5 * ( c ) ) - areacirculo ( c )
-- j) quadrante a partir das coordenadas
quadrante x y =	if ( ( x ) > 0 )
					then 	if ( ( y ) > 0 )
							then (1)
							else	if ( ( y ) < 0 )
									then (4)
									else (0)
					else	if ( ( x ) < 0 )
							then 	if ( ( y ) > 0 )
									then (2)
									else if ( ( y ) < 0 )
									then (3)
									else (0)
							else (0)