-- The answers for the first few exercises --

{-
Problem 1
=========
The first problem is solvable with list comprehensions. 
This single line is divisble in a few steps
1. take the list of all numbers 1-999
    [ x | x <- [1..999]
2. We add an extra 'constraint' to filter out all multiples of 3 and 5
    [ x | x <- [1..999], (x `mod` 5 == 0 || x `mod` 3 == 0)
3. We take the sum of all numbers in the list.
   sum[x | x <- [1..999], (x `mod` 5 == 0 || x `mod` 3 == 0)]

-}
answer1 = sum[x | x <- [1..999], (x `mod` 5 == 0 || x `mod` 3 == 0)]


{-
Problem 2
=========
-}

-- naive solution, this takes too long
naive_fib n | n < 2 = 1 | otherwise = naive_fib n-1 + naive_fib n-2
naive_fiblist = [ naive_fib x | x <- [1..100]]
naive_answer2 = [ x | x <- naive_fiblist, x < 4000000 && x `mod` 2 == 0]

{-
Since calculating the nth fibonacci-number with the recursive definition is expensive, 
I have made a function that builds a list and "remembers" all previous fibonacci-numbers.
This way we only have to calculate each number in the sequence only once.
-}

-- builds a smart list of fibonacci-numbers
fiblist :: Int -> Int -> [Int] -> [Int]
fiblist start end list	| start < 0 || end < 0 = []
						| start > end = list
						| start < 2 = fiblist (start+1) end (list ++ [1])
						| otherwise = fiblist (start+1) end (list ++ [ (list!!(start-1) + list!!(start-2))])

-- makes a list of the first n fibonacci-numbers
fib_up_to n = fiblist 0 (n-1) []

-- list of all even fibonacci-numbers smaller than 4M
even_fibs = [x | x <- fib_up_to 100, 0 < x && x < 4000000 && x `mod` 2 == 0]

answer2 = sum even_fibs

{- 
Problem 3
============ 
A bit harder. The trick to speed is a bit "dirty": I estimated where the highest prime factor would be. 
At first I checked the first 2000 primes, then 4000, then 6000...
largest_prime_factor foo (primes_up_to 2000) 0
largest_prime_factor foo (primes_up_to 4000) 0
largest_prime_factor foo (primes_up_to 6000) 0
largest_prime_factor foo (primes_up_to 7000) 0
-}
-- the sieve of Eratosthenes
sieve :: ([Int],[Int]) -> ([Int],[Int])
sieve ([],primes) = ([],primes) -- if the list of [2..n] is empty, we're done
sieve ((x:xs),primes) = ([ i | i <- xs, i `mod` x /= 0],primes++[x]) -- filter out all multiples of the first 'unmarked' number

-- returns a list of all primes up to n
primes_up_to n = snd((iterate sieve ([2..n],[]))!!n)

-- gets a tuple of n mod largest, largest prime factor (until now)
largest_prime_factor n [] largest = (n,largest)
largest_prime_factor n (x:xs) largest = if (n `mod` (toInteger x)) == 0 then largest_prime_factor (n `div` (toInteger x)) xs x else largest_prime_factor n xs largest

-- this function eventually did the trick
foo = 600851475143
ans3 = largest_prime_factor foo (primes_up_to 7000) 0



