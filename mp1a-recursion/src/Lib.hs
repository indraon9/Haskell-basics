--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake count _
 | count <=0 = []
mytake count (x:xs) = x : mytake (count -1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop count xs
 | count <=0 = xs
mydrop count (x:xs) = mydrop (count -1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev input = revHelper input []
 where
  revHelper :: [a] -> [a] -> [a]
  revHelper [] output = output
  revHelper (x:xs) output = revHelper xs (x:output)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] x = x
app x [] = x
app xs ys =
 let revFirstIn = rev xs
 in appHelper revFirstIn ys
  where 
   appHelper :: [a] -> [a] -> [a]
   appHelper [] x = x
   appHelper x [] = x
   appHelper (x:xs) out = appHelper xs (x:out)
 



--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []

inclist (x:xs) = (x+1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0

sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []

myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys = [x + y | (x, y) <- myzip xs ys]

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = natsHelper 0
 where
  natsHelper :: (Num a) => a -> [a]
  natsHelper x = x : natsHelper (x + 1)


--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)
  where
    addpairs (x:xs) (y:ys) = x + y : addpairs xs ys


--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x [] = [x]

add x (y:ys)
  | x == y = y:ys
  | x < y = x : y : ys
  | otherwise = y : add x ys


--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union x [] = x
union [] x = x

union (x:xs) (y:ys)
 | x < y = x : union xs (y:ys)
 | x > y = y : union (x:xs) ys
 | otherwise = x : union xs ys


--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect _ [] = []
intersect [] _ = []

intersect (x:xs) (y:ys)
 | x < y = intersect xs (y:ys)
 | x > y = intersect (x:xs) ys
 | otherwise = x : intersect xs ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (powerset xs) $ P.map (add x) (powerset xs)  


--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: (Num a) => [a] -> [a]
inclist' xs = P.map (+1) xs

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a 
sumlist' xs = P.foldr (+) 0 xs


--- #### New Fib

newFib :: Int -> Int

newFib 1 = 1
newFib 2 = 1
newFib a = newFib(a-1) + newFib(a-2)


--- #### New Fib Tail Recursion
fun3 n = aux n 1 1
 where aux 0 f1 _ = f1
       aux n f1 f2 = aux (n-1) f2 (f1+f2) 

--- #### Fun1

fun1 [] = 0
fun1 (x:xs) |even x = fun1 xs - 1
            |odd x = fun1 xs + 1






