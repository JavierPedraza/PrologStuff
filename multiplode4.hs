mult4 1 = False
mult4 n = if((mod0 n 4) == 0) then True
            else False

contarMult4 1 = 0
contarMult4 n = if((mult4 n) == True) then 1 + (contarMult4 (n-1))
                    else 0 + (contarMult4 (n-1))

mod0 n 0 = 0
mod0 n d = if(d>n) then n
            else mod0 (n-d) d