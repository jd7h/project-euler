import Data.List

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

{- 
Problem 4
=========

Find the largest palindrome made from the product of two 3-digit numbers.
-}

-- first, lets consider the biggest possible product of two three digit numbers.
-- biggestProduct = 999 * 999


-- we can write these products x*y as tuples (x,y)
productTuples = reverse [ (x,y) | x <- [1..999], y <- [1..999]]


-- number palindrome
isPalindromic n = isPalindrome (show n)

-- string palindrome 
isPalindrome :: String -> Bool
isPalindrome s = if (length s < 2 || head s == last s) 
						then (length s <= 2 || isPalindrome (tail (init s)))
						else False

-- find the biggest number in a list
maxInt :: [Int] -> Int -> Int
maxInt [] y = y
maxInt (x:xs) y = if x > y then maxInt xs x else maxInt xs y

--Find the biggest palindrome
biggestPalindrome :: Int
biggestPalindrome = maxInt [ (x*y) | (x,y) <- productTuples, isPalindromic (x*y)] 0

{- Problem 5
============ -}

goal = [2.20]
--note that if a number is divisible by 20, it is also divisible by 10, 5, 2.
--this gives us a smaller list:
new_goal = [11,12,13,14,15,16,17,18,19,20]


{-
these are apparently expensive functions. 
I didn't know there's already a faster implementation in Haskell.
To make the list of all numbers divisible by n, it's better to take [n,2n,..]

step_with with = iterate (+with) with

step_with_to with to = step_with_to' with to [with]
    where step_with_to' with to xs
           | last xs + with > to = xs
           | otherwise = step_with_to' with to (xs ++ [last xs + with])

step_from_with_to from with to = step_from_with_to' from with to [from]
    where step_from_with_to' from with to xs
           | (last xs + with) <= to = step_from_with_to' from with to (xs ++ [last xs + with])
           | otherwise = xs

mults_from_div_to from div to
	| (from `mod` div == 0) 	= mults_from_div_to' from div to [from] 
	| otherwise 				= mults_from_div_to' from div to [from + (div - (from `mod` div))]
	where
		mults_from_div_to' from div to xs
			| last xs + div > to		= xs
			| otherwise					= mults_from_div_to' from div to (xs ++ [last xs + div])
-}
            
--intersection is also a costly operations. Let's not use it.

searchlist = [20,40..999999999]

filter_it [] fs = []
filter_it xs [] = xs
filter_it xs (f:fs) = filter_it [x | x <- xs, x `mod` f == 0] fs

ans5 = filter_it searchlist new_goal

{-
Problem 6
==========
-}

-- a better (real) sieve than the one used before
--returns a list of the first i primes
psieve i nats primes
    | null nats					= []
    | null primes               = psieve i nats [2]
	| length primes < i 		= psieve i new_nats (primes ++ [head new_nats])
	| otherwise					= primes
		where new_nats = [x | x <- nats, x `mod` last primes /= 0]

--returns the ith prime
psieve2 i nats prime
    | null nats					= -1
	| i <= 1			 		= prime 
	| otherwise					= psieve2 (i-1) new_nats (head new_nats)
		where new_nats = [x | x <- nats, x `mod` prime /= 0]

--solution (slow)
--I knew where to look because of the prime counting function
ans6 = psieve2 10001 [2..105000] 2
