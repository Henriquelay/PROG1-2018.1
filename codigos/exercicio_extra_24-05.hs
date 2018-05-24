{- Dada uma lista de inteiros, verifique se estão em ordem cresente (admitindo valores repetidos) -}
isNaoDecrescente xs = length [ xs!!i | i <- [0..(length xs -2)], xs!!i <= xs!!(i+1) ] == length xs -1
isNaoDecrescenteAlt xs = null [ xs!!i | i <- [0..(length xs -2)], xs!!i > xs!!(i+1) ]
isPrimo n = [n] == [ r | r<-[2..n], mod n r == 0]