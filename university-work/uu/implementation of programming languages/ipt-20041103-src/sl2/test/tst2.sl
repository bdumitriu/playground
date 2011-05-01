let fac :: Int -> Int =
		\n -> if n > 0 then n * fac(n-1) else 1 fi
 in fac 4
 ni