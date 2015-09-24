> import Data.List

Course
==============

Anonymous f'n / lambda abstraction: used to define a f'n without a name, if for ex we'll only use it once

> greaterThan100 :: [Int] -> [Int]
> greaterThan100 ls = filter (\x -> x > 100) ls

In this case, we could use an operator section. With ? an operator, ?x == \x -> y ? x, x? == \x -> x ? y.

> greaterThan100' ls = filter (>100) ls
> multBySix :: [Int] -> [Int]
> multBySix = map (*6)

> applyLatterToFormer :: (b -> c) -> (a -> b) -> (a -> c)
> applyLatterToFormer f f' = \x -> f (f' x)

That's f'n composition. The true applyLatterToFormer is (.) in Haskell

> myTest xs = even (length (greaterThan100 xs))

... is really the same as ...

> myTest' = even . length . greaterThan100

Then there's stuff about how all Haskell f'ns only take one argument but you already know all that: Haskell f'ns take only one arg and return a f'n.

A couple tests:

> largestDivider :: (Integral a) => a -> a
> largestDivider n = head $ filter (\x -> x `mod` n == 0) [100000,99999..1]

Sum of all odd squares that are smaller than 10 000

> sumDescribedAbove :: (Integral a) => a
> sumDescribedAbove = sum $ takeWhile (<10000) (map (^2) [1,3..])
> sumDescribedAbove' = sum $ filter odd (takeWhile (<10000) (map (^2) [1..]))

Collatz sequences: even num divided by two, odd x gives 3x+1. Apply same to resulting num, until 1 (any starting number eventually gives 1)

> collatzSequence :: (Integral a) => a -> [a]
> collatzSequence 1 = [1]
> collatzSequence n
>     | odd n     = 3*n+1:collatzSequence (3*n+1)
>     | otherwise = n `div` 2:collatzSequence (n `div` 2)

Now: for all numbers between 1 and 100, how many produce a chain of length >15 ?

> howManyCollatz :: (Num a) => a
> howManyCollatz = fromIntegral $ length $ filter (\x -> length x > 15) (map collatzSequence [1..100])

Left and right folds (I'm just doing the LYAH Higher Order chapter at this point)
foldl folds a list (or foldable? well, I don't know what a foldable is for now, so let's stick with list) from the LEFT (starting with the head). foldr does so from the right.

foldl takes a binary f'n, a starting value and a list as parameters. This is sum with foldl:

> sum' :: (Num a) => [a] -> a
> sum' xs = foldl (\acc x -> acc + x) 0 xs

Way more succent: we know (+) takes 2 parameters so yeah, the starting value and the element of the list, and adds them up. Because of curried f'ns, no need to add the list. Here sum'' is defined as a f'n which takes a list. Generally speaking, foo a = bar b a can be rewritten foo = bar b

> sum'' :: (Num a) => [a] -> a
> sum'' = foldl (+) 0

> elem' :: (Eq a) => a -> [a] -> Bool
> elem' x = foldl (\acc y -> if x == y then True else acc) False

> map' :: (a -> b) -> [a] -> [b]
> map' f = foldr (\x acc -> f x : acc) [] 

Right folds work on infinite lists, lefts don't. Logical: if you take an infinite list at some point and work from it from the right, you'll eventually end up at the beginning. With left folds you'll never see the end.
foldl1 and foldr1 are the same expect the starting element is the first element encountered. But, runtime error when given the empty list so be pretty careful.

> maximum' :: (Ord a) => [a] -> a
> maximum' = foldl1 (\acc x -> if x > acc then x else acc)
> reverse' :: [a] -> [a]
> reverse' = foldl (\acc x -> x:acc) []
> -- reverse' = foldl (flip (:)) []
> product' :: (Num a) => [a] -> a
> product' = foldl1 (*)
> filter' :: (a -> Bool) -> [a] -> [a]
> filter' p = foldl (\acc x -> if p x then x:acc else acc) []
> head' :: [a] -> a
> head' = foldr1 (\x _ -> x)

Misc: filter doesn't work on infinite lists. At all. We usually use takeWhile instead of it when we know that the first False means that everything else is False.

About $ : it's function application, like a space. But the space has the highest precedence, $ has the lowest. f a b c is like (((f a) b) c) but f $ a b c applies f to the whole a b c thing. So when encountering $, everything on it's right will be given to the f'n left, saving us the trouble of using parenthesis. Two equivalent statements:

> example  = sum (map sqrt [1..130])
> example' = sum $ map sqrt [1..130]

You can imagine $ as opening a parenthesis and putting a closing one at the far right of the expression. It is also useful to map parameters to a list of f'ns, for example:

> example'' :: [Int]
> example'' = map ($ 3) [(*2),(3+),(^2)]

(.) is for f'n composition like in math, talked about it earlier (and still not fully understand how it's different from simply enchaining $s, so take a look at this when there is time).

curry is used to go from f'n taking a pair as argument (so two arguments in a sense) to the curried way of taking 2 args, which is a f'n taking a f'n taking an argument. Uncurry is the opposite: try for ex "uncurry (+) (1,3)" in ghci, which takes the curried f'n (+) that we know about, and modifies it such that it takes a pair as arg, then applying that to the pair (1,3): this is when uncurry is usually useful

> schonfinkel :: ((a,b) -> c) -> a -> b -> c
> schonfinkel f a b = f (a,b)
> unschonfinkel :: (a -> b -> c) -> (a,b) -> c
> unschonfinkel f (a,b) = f a b

Composition => chaining f'ns. Take a look at this:

> foobar :: [Integer] -> Integer
> foobar []           = 0
> foobar (x:xs)
>         | x > 3     = (7*x+2) + foobar xs
>         | otherwise = foobar xs

This seems pretty good right? WRONG.
This is working on a too low level, and my foobar f'n does too much. Here is good haskell style:

> foobar' :: [Integer] -> Integer
> foobar' = sum . map (\x -> 7*x+2) . filter (>3)

Here, not working individually with each element, but we instead think about the transformation going through the whole input. My foobar is a pipeline of 3 other f'ns, this is composition, just a sort of pipeline. No explicit recursion pattern here. This is what is called wholemeal programming.

HW
=============

Exercise 1 :

> fun1 :: [Integer] -> Integer
> fun1 []         = 1
> fun1 (x:xs)
>     | even x    = (x - 2) * fun1 xs
>     | otherwise = fun1 xs
> fun1' :: [Integer] -> Integer
> fun1' = product . map (subtract 2) . filter (even)

> fun2 :: Integer -> Integer
> fun2 1 = 0
> fun2 n | even n    = n + fun2 (n `div` 2)
>        | otherwise = fun2 (3 * n + 1)
> fun2' :: Integer -> Integer
> fun2' = sum . filter even . takeWhile (/=1) . iterate transformation
>     where transformation n | even n    = n `div` 2 
>                            | otherwise = 3*n+1

Exercise 2 :

> data Tree a = Leaf
>             | Node Integer (Tree a) a (Tree a)
>     deriving (Show, Eq)
> 
> foldTree :: [a] -> Tree a
> foldTree = foldr construct Leaf
> construct :: a -> Tree a -> Tree a
> construct elem Leaf = Node 0 Leaf elem Leaf
> construct elem (Node a tInf b tSup)
>     | height tInf < height tSup = let newNode = construct elem tInf in Node (height newNode + 1) newNode b tSup
>     | otherwise                 = let newNode = construct elem tSup in Node (height newNode + 1) tInf b newNode
>     where height :: Tree a -> Integer
>           height Leaf      = -1
>           height (Node n _ _ _) = n

To test out my result (call this f'n inside putStr) :

> displayTree :: (Show a) => Tree a -> String
> displayTree Leaf             = "Leaf"
> displayTree (Node n t1 a t2) = "Node number " ++ show n ++ ": " ++ show a ++ ". Sub nodes: \n\t" ++ displayTree t1 ++ "\nAND " ++ displayTree t2 ++ "\n"
>

Exercise 3 :

> xor :: [Bool] -> Bool
> xor = foldr (\x acc -> if not x then not acc else acc) True

Implemented above:
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

> myFoldl :: (b -> a -> b) -> b -> [a] -> b
> myFoldl f base = foldr (flip f) base . reverse 

Exercise 4 : implemented not using what the teacher was referring to (cartesian prod), but this website explaining the algorithm : https://plus.maths.org/content/sundarams-sieve

> sieveSunduram :: Integer -> [Integer]
> sieveSunduram n = let array = map head $ group $ sort $ generateArray 4 3 n in [2*x+1 | x<-[0..n] , not (x `elem` array)]
> generateArray :: Integer -> Integer -> Integer -> [Integer]
> generateArray start ecart n = if n > start then [start,start+ecart..n] ++ (generateArray (start+3) (ecart+2) n) else []

