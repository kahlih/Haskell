-- Kahli Holmes EID: kh27624


--fibs creates an infinite list containing members of the
--fibonacci sequence
-------------------------------------------------------------
fib = 0 : 1 : zipWith (+) (fib) (tail fib)

fibs :: [Int]
fibs = tail fib
-------------------------------------------------------------


--primes creates an inifinite list of prime numbers
-------------------------------------------------------------
isPrime n = go 2
  where
    go d
      | d*d > n        = True
      | n `mod` d == 0 = False
      | otherwise      = go (d+1)

primes :: [Int]
primes = filter isPrime [2 .. ]
-------------------------------------------------------------


--an infinite list that contains the Fibonacci numbers whose
--position corresponds to a prime number
-------------------------------------------------------------
indexC :: [Int] -> [Int]
indexC [] = [] --Should never reach this case (infinite list)
indexC (e:l) = (fibs !! (e-1)) : indexC l

partC :: [Int]
partC = indexC primes 
-------------------------------------------------------------


--an infinite list that contains the prime numbers whose 
--position corresponds to a Fibonacci number
-------------------------------------------------------------
indexD :: [Int] -> [Int]
indexD [] = [] --Should never reach this case (infinite list)
indexD (e:l) = (primes !! (e-1)) : indexD l

partD :: [Int]
partD = indexD fibs
-------------------------------------------------------------


-- Main function 
main = do
    print (take 10 fibs)
    print (take 10 primes)
    print (take 10 partC)
    print (take 10 partD)

    