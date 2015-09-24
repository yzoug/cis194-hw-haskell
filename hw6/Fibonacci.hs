{-# LANGUAGE FlexibleInstances #-}

--Ex 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib x|x<-[0..]]
--or, with map: fibs1 = map fib [0..]

--Ex 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

--Ex 3
data Stream t = Cons t (Stream t)

buildStreamOfOnes :: (Num a) => Stream a --just to test it out
buildStreamOfOnes = Cons 1 buildStreamOfOnes

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
    show strm = show $ take 20 $ streamToList strm

--Ex 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons s sx) = Cons (f s) (streamMap f sx)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed rule seed = Cons seed (streamFromSeed rule (rule seed))

--Ex 5
nats :: Stream Integer
nats = Cons 1 (streamMap (+1) nats)

powersOfTwo :: [Integer]
powersOfTwo = [2^x|x<-[0..]]

ruler :: Stream Integer
ruler = streamMap testFn nats
    where testFn :: Integer -> Integer
          testFn x | odd x     = 0
                   | otherwise = last $ takeWhile (\y -> even (x `div` y)) powersOfTwo

--Ex 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
        --fromInteger :: Integer -> (Stream Integer)
        fromInteger n = Cons n (streamRepeat 0) 
        --negate :: (Stream Integer) -> (Stream Integer)
        negate strm = streamMap (* (-1) ) strm
        --(+) :: (Stream Integer) -> (Stream Integer) -> (Stream Integer)
        (+) (Cons x1 strm1) (Cons x2 strm2) = Cons (x1+x2) (strm1+strm2)
        --(*) :: (Stream Integer) -> (Stream Integer) -> (Stream Integer)
        (*) (Cons a0 a') bb@(Cons b0 b') = (Cons (a0*b0) (streamRepeat 0)) + (Cons 0 ((fromInteger a0)*b' + a'*bb))

