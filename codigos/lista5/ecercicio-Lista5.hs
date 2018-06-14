somarecursao xs = if null xs then 0 else head xs + somarecursao (tail xs)

maiorElem xs = if null (tail xs) || null xs then xs
                else if head xs >= last xs
                        then maiorElem (init xs)
                        else maiorElem (tail xs)

opBoolOU xs = if elem True xs then True else False

opBoolE xs = if not (elem False xs) then True else False

elemRecur x xs = if null xs then False
                    else if head xs == x
                            then True
                            else elemRecur x (tail xs)

menoresRecur x xs = if null xs then []
                    else if head xs < x
                            then [head xs] ++ menoresRecur x (tail xs)
                            else menoresRecur x (tail xs)


filtro xs pred = [ x | x<-xs, pred x]

listaEven xs = filtro xs even

filtroRecur xs pred =  if null xs then []
                        else if pred (head xs)
                                then [head xs] ++ filtroRecur (tail xs) pred
                                else filtroRecur (tail xs) pred

dWhileRecur xs cond = if null xs then []
                        else if cond (head xs)
                                then [head xs] ++ tWhileRecur (tail xs) cond
                                else []

                                tWhileRecur xs cond = if null xs then []
                                else if cond (head xs)
                                        then []
                                        else [head xs] ++ tWhileRecur (tail xs) cond