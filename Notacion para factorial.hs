--En haskell los programas se componene de una serie de funciones
--UNA FUNCION: está definicda por varias reglas de reescritura
--a la izquierda del igual esta la cabeza de la funcion
--a la derecha del igual esta el cuerpo de la funcion
--El orden de las reglas especifica prioridad de matching
--(hay que poner el caso particular primero)
--Los argumentos y llamada a funcion an en notacion currificada
--en lugar de: f(a1,a2,(a3-1))	se escribe: 	f a1 a2 (a3-1)
--Toda definicion de funcion comienza en la columna 1, se debe indentar



--Toda definicion de funcion comienza en la columna 1, y se debe indentar

factorial 0 = 1
factorial n = n * (factorial (n-1))

factorial2 n = if(n==0) then 1
                else n * (factorial2(n-1))

factorial3 n
		| (n==0) = 1
		| (n>0) = n * (factorial3(n-1))

factorial4 n
		| (n==0) = 1
		| otherwise = n * (factorial3(n-1))