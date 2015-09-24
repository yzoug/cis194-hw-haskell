> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
> {-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

Course
============
Type classes: Num Eq Ord Show etc. that I already kind of know about, they are set of types which have certain operations defined for them. For Eq for ex, the operations (==) and (/=) are defined. So every type that want to be in Eq needs to define those two operations.
In "(==) :: Eq a => a -> a -> Bool", it means that for == to work, the type a we choose needs to be in the class Eq. The compiler then chooses which implementation of == it is going to use, in f'n of the type a we chose (like overloaded methods in Java).
Of course, we can define our own type structures and make them *part of* a type class. Let's do just that with Eq:

> data Foo = F Int | G Char
> instance Eq Foo where
>     (F i1) == (F i2) = i1 == i2
>     (G c1) == (G c2) = c1 == c2
>     _ == _           = False
>     foo1 /= foo2     = not (foo1 == foo2)

Now, here, we define manually (==) and (/=) which is a bit annoying. In fact, the class Eq is defined such that "(==)" is "not (/=)" and "(/=)" is "not (==)". So we can choose and define either one, the other is automatically deduced. And of course, if for some reason we don't want that behaviour, we can define them both.
But with Eq, we could do something even easier, let the compiler do the work. In fact, it's what we've been doing previously:

> data Foo' = F' Int | G' Char
>     deriving (Eq, Ord, Show)

Like that, GHC will automatically derive instances of Eq Ord and Show type classes for our data type Foo'.

We can ofc define our own type class (like Eq or Ord). Let's try Listable, a typeclass that would work with everything that can be converted to a list of Ints:

> class Listable a where
>     toList :: a -> [Int]

Ok, so now the f'n toList is defined and has type "Listable a => a -> [Int]" . But ofc we need to define how it behaves, and for now, no type is in our Listable. We add them by defining instances:

> instance Listable Int where
>     --toList :: Int -> [Int]
>     toList x = [x]

Now, by supplying an int to the f'n toList, it'll create automatically the [Int] that we expect using what we defined (so for an Int autoamtically create the singleton list associated). Let's add other things in Listable.

> instance Listable Bool where
>     toList True = [1]
>     toList _    = [0] 
> instance Listable [Int] where
>     toList = id --means don't do anything

Let's say that our data structure Tree can be listable.

> data Tree a = Empty | Node (Tree a) a (Tree a)
> instance Listable (Tree Int) where
>     toList Empty = []
>     toList (Node l x r) = toList l ++ [x] ++ toList r

However all this requires FlexibleInstances (see top of file). Else you can't define an explicit type (like Int here) but you need to use an already existing typeclass (Num, Integral etc)

HW
============
See Calc.hs

