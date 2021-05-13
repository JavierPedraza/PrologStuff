import Data.Char
--Dada una fase encriptarla sumandole un valor 3 a su ordinal
encriptar cadena = 
			let
				ordinales = [(ord c) | c<-cadena]
				encriptado =  [(o+2) | o<-ordinales]
				fraseEnc = [(chr e) | e<-encriptado]
			in 
				fraseEnc


desencriptar cadena =
			let
				ordinales = [(ord c) | c<-cadena]
				desencriptado =  [(o-2) | o<-ordinales]
				fraseDesenc = [(chr e) | e<-desencriptado]
			in 
				fraseDesenc


list1 = [1,3,5,7]

list2 = [2,4,6,8]

prodCart = [(x,y) | x <- list1, y <- list2]



--ARBOLES


--Arbol Binario
data Arbol = Hoja Int | Rama Int Arbol Arbol

arbolito = (Rama 1 (Rama 2 (Rama 3 (Hoja 4) (Hoja 5)) (Hoja 6))(Hoja 7))

sumarelementos (Hoja x) = x
sumarelementos (Rama x ri rd) = x + (sumarelementos ri) + (sumarelementos rd)
