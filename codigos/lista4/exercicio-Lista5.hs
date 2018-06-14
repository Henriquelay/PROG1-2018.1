foo0 = compare 2 (mod 17 2) == EQ --dizendo se 2 é ou não é igual ao resto da divisão interira de 17 por 2
foo1 = x + y --somando 2 com 2
        where 
        x = y
        y = 2
foo2 y = x + y --a ideia é semelhante a foo1, mas há um problema de sintaxe : há um where dentro de um where, o Y do segundo where "não é enxergado na função foo2"
        where 
        x = y
            where 
            y = 2
foo3 x = if( x <= 30 ) --responde D se o número é <=30, caso não seja, C se é <= 50, caso não seja, B se é <= 80, caso não seja, A
        then 'D'
        else if( x <= 50 ) 
            then 'C'
            else if( x <= 80 ) 
                then 'B'
                else 'A'

{- ----------------- Exercício A ----------------- -}
{- Item 1 -}
{- A a E  -}
mult5gt0lt80 = [5,10..75]
mesesAno = ["janeiro","fevereiro","marco","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro"]
numeroDias = [31,28,31,30,31,30,31,31,30,31,30,31]
diasSemana = ["domingo","segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira","sábado"]
listaDisciplinas = ["calculo 1","algebra linear","programacao 1","introducao a computacao","aspectos teoricos da computacao"]
{- Item 2 -}
f x r t = [x, x + r .. t]

{- Item 3 -}
{- a -} {- Dada que a lista é não infinita -}
menorLista x =  if length x > 0 
                then menor x 
                else error "Por favor entre uma lista nao nula"  
{- Módulos (podem ser uteis depois) -}
menor x
    | head x <= last x = head x
    | head x > last x = last x

maior x
    | head x >= last x = head x
    | head x < last x = last x
{- ------------------------------------ -}

{- b -}
maioremenor xs = (maior xs,menor xs)

{- c -}
multiplos n lim = [n*1,n*2..lim]
--ERROR-- multiplosAlternativo n lim = [x*n|x<-[0,1..],x <= lim]

{- d -}
metades xs = (take (metadeDaListaInt xs) xs,drop (metadeDaListaInt xs) xs)
{- Módulo (pode ser útil futuramente) -}
metadeDaListaInt xs = div(length xs) 2
{- ---------------------------------- -}

{- e -}
duplica xs = [ xs!!div n 2 | n <- [0..2*(length xs)-1]]