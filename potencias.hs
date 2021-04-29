p0 n 0 = 1
p0 n p = n * (p0 n (p-1))

p1 n p = if (p == 0) then 1
        else n * (p1 n (p-1))

p2 n p
        | (p==0) = 1
        | (p>0) = n * (p2 n (p-1))

p3 n p 
        | (p == 0) = 1
        | otherwise = n * (p3 n (p-1))