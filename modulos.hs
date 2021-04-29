mod0 n 0 = 0
mod0 n d = if(d>n) then n
            else mod0 (n-d) d

mod1 n d
    | (d == 0) = 0
    | (d > n ) = n
    | otherwise = mod1 (n-d) d