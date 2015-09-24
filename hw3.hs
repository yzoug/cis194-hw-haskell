import Data.List

--- Course
-- First, exprerimenting with concepts of course. Then homework section.

data IntList = Empty
             | Cons Int IntList
    deriving Show

squareAll :: Int -> Int
squareAll x = x*x

workOnList :: (Int -> Int) -> IntList -> IntList
workOnList _ Empty       = Empty
workOnList f (Cons x xs) = Cons (f x) (workOnList f xs)
--i shouldve name it "mapIntList"

keepOnlyPositive :: IntList -> IntList
keepOnlyPositive Empty       = Empty
keepOnlyPositive (Cons x ls) = if x>=0 then Cons x (keepOnlyPositive ls) else keepOnlyPositive ls
-- or: keepOnlyPositive (Cons x ls)
--         | x > 0     = Cons x (keepOnlyPositive ls)
--         | otherwise = keepOnlyPositive xs
--

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty       = Empty
keepOnlyEven (Cons x ls)
    | even x    = Cons x (keepOnlyEven ls)
    | otherwise = keepOnlyEven ls

--generalization of keepOnly f'ns (called filters) :
filterIntList :: IntList -> (Int -> Bool) -> IntList
filterIntList Empty _ = Empty
filterIntList (Cons x ls) test
    | test x    = Cons x (filterIntList ls test)
    | otherwise = filterIntList ls test
--the f'n test is called a predicate (Int -> Bool f'n)

--polymorphic data type
data List t = E --E for Empty C for Cons already used earlier
            | C t (List t)
--t is a type variable that could be any type.

example1 :: List Int
example1 = C 7 (C 42 (C 13 E))

example2 :: List Char
example2 = C 'L' (C 'O' (C 'L' E))

filterList :: (t -> Bool) -> List t -> List t
-- for any type t
filterList _ E = E
filterList f (C a lst)
    | f a       = C a (filterList f lst)
    | otherwise = filterList f lst

--mapList :: (t -> t) -> List t -> List t
--ok, I thought of that but actually it's an over restriction
--this means that the List we have at the end is composed of elements
--that need to have the same types as in the first list
--we wouldn't be able to construct, for ex, a list of Bool
--from a list of Int. This is the non restrictive type sig:
mapList :: (a -> b) -> List a -> List b
--btw this doesn't, ofc, exclude type a = type b
mapList _ E         = E
mapList f (C a lst) = C (f a) (mapList f lst)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL x _) = x

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ xs) = xs

--- Homework
--

skips :: [a] -> [[a]]
skips lst = map (everyNth lst) [1,2..length(lst)]
    where everyNth :: [a] -> Int -> [a]
          everyNth xs n = case drop (n-1) xs of []   -> []
                                                y:ys -> (y:everyNth ys n)

localMax :: [Integer] -> [Integer]
localMax (x:y:z:xs) = if y>x && y>z then y:localMax (y:z:xs) else localMax (y:z:xs)
localMax _          = []

--histogram :: [Int] -> String
--histogram xs = construct (map length (group (sort xs))) ++ "==========\n0123456789\n"
--    where construct :: [Int] -> String
--          construct []        = let xss = map (subtract 1) xs in "\n" ++ construct (map length (group (sort xss)))
--          construct ls@(z:zs) = if notAllNegative ls then
--                                    if z > 0 then "*" ++ construct zs
--                                    else " " ++ construct zs
--                                else "\n"
--              where notAllNegative :: [Int] -> Bool
--                    notAllNegative []      = False
--                    notAllNegative (n:lst) = if n > 0 then True else notAllNegative lst

histogram :: [Int] -> String
histogram ls = (unlines $ reverse $ transpose $ construct $ prepare ls) ++ "==========\n0123456789\n"

construct :: [Int] -> [String]
construct []     = []
construct (x:xs) = (if x>0 then replicate x '*' else [' ']):construct xs

prepare :: [Int] -> [Int]
prepare xs = map (length') (group (sort (f'n xs)))
    where length' ls = length ls - 1

f'n :: [Int] -> [Int]
f'n ls = [0,1,2,3,4,5,6,7,8,9] ++ ls

