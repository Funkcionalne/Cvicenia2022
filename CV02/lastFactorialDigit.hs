lnd n = let mod10 = n `mod` 10 in if  mod10 > 0 then mod10 else lnd (n `div` 10)

-- zle len 2*5 = 10
lnfd 1 = 1
lnfd n = lnd (x*n) where x = lnfd (n-1)

logd d n = if n `mod` d == 0 then let (a,b) = logd d (n `div` d) in (a, b+1) else (n, 0)

posledna n = lastD n 0 0 1
lastD n p2 p5 f | n == 1 && p2 == p5  = f
                | n == 1 = lastD 1 (p2-1) p5 (lnd (f * 2))
                | n `mod` 5 == 0 = let (a,b) = logd 5 n in lastD (n-1)  p2 (p5+b) (lnd (f * (lnd a)))
                | n `mod` 2 == 0 = let (a,b) = logd 2 n in lastD (n-1) (p2+b) p5  (lnd (f * (lnd a)))
                | otherwise      = lastD (n-1) p2 p5 (lnd (f * (lnd n)))


e = [posledna n == lnd (lastDfd n) | n <- [1..4000]]
lastDfd :: Int -> Int       
lastDfd 0 = 1
lastDfd 1 = 1
lastDfd 2 = 2
lastDfd 3 = 6
lastDfd 4 = 4
lastDfd 5 = 2
lastDfd 6 = 2
lastDfd 7 = 4
lastDfd 8 = 2
lastDfd 9 = 8
lastDfd n | odd ((n `div` 10) `mod` 10) = 4 * lastDfd(n `div` 5) * lastDfd(n `mod` 10)
         | otherwise = 6 * lastDfd( n `div` 5 ) * lastDfd(n `mod` 10)