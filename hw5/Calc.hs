{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Calc where

import Parser

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
    deriving Eq
instance Show ExprT where
    show (Lit a)   = show a
    show (Add a b) = "(" ++ show a ++ ") + (" ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ ") x (" ++ show b ++ ")"

--Ex1
eval :: ExprT -> Integer
eval (Lit a)   = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

--Ex2
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of Nothing -> Nothing
                                               Just a  -> Just (eval a)

--Ex3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

--Ex4
newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)
newtype Mod7   = Mod7 Integer deriving (Show, Eq)

instance Expr Integer where
    lit = id 
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit a = MinMax a
    add   = max
    mul   = min

instance Expr Mod7 where
    lit a                 = Mod7 (a `mod` 7)
--    add   = (+) . (`mod` 7) FAUX, ne fonctionne pas comme prévu
--    mul   = (*) . (`mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (`mod` 7) $ (a+b)
    mul (Mod7 a) (Mod7 b) = Mod7 $ (`mod` 7) $ (a*b)

