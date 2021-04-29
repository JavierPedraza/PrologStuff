mod0 n 0 = 0
mod0 n d = if(d>n) then n
            else mod0 (n-d) d

--Pattern Matching
--Es un mecanismo que permite tener dos versiones (reglas) distintas 
--distintas de una definicion de función al ejecutarse
--una llamada el Intérprete escoje aquella que hace
--emparejamiento de patrones
primo::Integer -> Bool
primo 1 = False
primo n = let
                nveces = cuentaDiv n n
            in
                if (nveces > 2) then False
                    else True

cuentaDiv n 1 = 1
cuentaDiv n c = if((mod0 n c) == 0) then 1 + (cuentaDiv n (c-1))
                    else 0 + (cuentaDiv n (c-1))

cuentaPrim 1 = 0
cuentaPrim n = if((primo n) == True) then 1 + (cuentaPrim(n-1))
                    else 0 + (cuentaPrim(n-1))

