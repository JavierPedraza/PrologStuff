--Notacion explicita de Listas en Haskell
--
--
--[1,2,3,4,5]
--
--O indicando un intervalo
--[1,3...15]
--Esto indica un aumento de 3 en 3 hasta llegar al numero 15
--
--Notacion Constructora de Listas
--
--
--3:[4,5]
--el operador ":" indica que el 3 se va a incrustar en la lista [4,5]
--lo cual resulta en [3,4,5]
--1:7:[4,5] resulta en [1,7,4,5]

--Determinar si el numero siete esta en una lista de enteros

esta7 [] = False
esta7 (x:xs) = if(x == 7) then True
                else esta7 xs
--[x:xs] indica una lista no vacia, donde x es el primer elemento y xs es
--una lista donde estan el resto de los elementos, puede estar vacia o no

--Contar cuantos Impares hay en una lista
impares [] = 0
impares (x:xs) = if((mod0 x 2) == 0) then 0 + (impares xs)
                    else 1+(impares xs)


--Sumar los pares de una lista
sumpar [] = 0
sumpar (x:xs) = if((mod x 2 ) == 0) then x + (sumpar xs)
                    else 0 + (sumpar xs)

--Sumar los numeros que estan en posicion impar de una lista
sumpos[] n = 0
sumpos (x:xs) n = if((mod0 n 2) == 0) then 0 + (sumpos xs (n+1))
                    else x + (sumpos xs (n+1))


--suma los valores de una lista cuya posicion es multiplo de n

sumposn l n = sumposn2 l n 0
sumposn2 [] n d = 0
sumposn2 (x:xs) n d = if((mod0 d n) == 0) then x + (sumposn2 xs n (d+1))
                    else 0 + (sumposn2 xs n (d+1))

--Borrar de una lista los numeros impares
borrarImpares:: [Integer]->[Integer]
borrarImpares[] = []
borrarImpares(x:xs) = if (odd x) then borrarImpares xs
                        else x:(borrarImpares xs)

--Dada una lista borrar los multiplos de 7 y los multiplos de 9
procesaL lista = let
                    lista1 = borram7 lista
                    r = borram9 lista1
                in
                    r

borram7 [] = [] 
borram7 (x:xs) = if ((mod0 x 7) == 0) then borram7 xs
                    else x:(borram7 xs)

borram9 [] = [] 
borram9 (x:xs) = if ((mod0 x 9) == 0) then borram9 xs
                    else x:(borram9 xs)


--Codigo para borrar el mayor y el menor numero de una lista
borraMenorMayor lista = let 
                            men = menor lista
                            may = mayor lista
                            lista1 = borraNum lista men
                            resultado = borranum lista1 may
                        in
                            resultado

mayor (x:xs) = mayorLista xs x
menor (x:xs) = menorLista xs x

menorLista [] menor = menor
menorLista (x:xs) menor = if (x < menor) then menorLista xs x
                            else menorLista xs menor

mayorLista [] mayor = mayor
mayorLista (x:xs) mayor = if (x > mayor) then mayorLista xs x
                            else mayorLista xs mayor

borraNum [] num  = []
borraNum (x:xs) num = if (x == num) then borraNum xs x
                        else x:(borraNum xs num)


--Proceso para ordenar una lista
--ordena [] = []
--ordena (x:xs) = let 
--                    m = mimenor (x:xs)
  --                  la = quitax (x:xs)
--
  --              in

quitax [] e = []
quitax (x:xs) e = if(e == x) then xs
                    else x:(quitax (xs) e)

mimenor (x:xs) = menn (x:xs) x
men [] v = v
men (x:xs) v = if (v <= x) then men xs v
                    else xs x


--Dada una frase regresar su valor en ordinal (numeros)


--Funcion de modulo

mod0 n 0 = 0
mod0 n d = if(d>n) then n
            else mod0 (n-d) d


