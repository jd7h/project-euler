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
answer = sum[x | x <- [1..999], (x `mod` 5 == 0 || x `mod` 3 == 0)]
