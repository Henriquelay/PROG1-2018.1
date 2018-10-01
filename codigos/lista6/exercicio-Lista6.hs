ehPref xs ys = take (length xs) ys == xs

ehPrefLista xs ys = if xs == ()

ehPrefRecur xs ys = if null xs || null ys 
                    then True
                    else if head xs == head ys
                        then ehPrefRecur (drop 1 xs) (drop 1 ys)
                        else False

ehSuf xs ys = reverse (take (length xs(reverse xs)) ys) == xs

ehSufRecur xs ys = if null xs || null ys 
                    then True
                    else if last xs == last ys
                        then ehSufRecur (init xs) (init ys)
                        else False