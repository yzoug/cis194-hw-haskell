--- Course
{-
 - Everything below this is just experimenting
 - with the course, then homework section
 -}
data Famille = Pere --simple enumeration
             | Mere
             | Fils
             | Fille
             deriving Show --so that it can be displayed

isMale :: Famille -> Bool
isMale Mere  = False
isMale Fille = False
isMale _     = True

data FailableDouble = Failure --algrebraic data type
                    | OK Double
                    deriving Show

var1 :: FailableDouble
var1 = Failure

var2 :: FailableDouble
var2 = OK 4.2

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

data Person = Person String Int Famille --possible to have more than 2 arguments
            deriving Show

zoug :: Person
zoug   = Person "Yassine" 18 Fils

latifa :: Person
latifa = Person "Latifa" 51 Mere

{- In general, this is algrebraic data structures:
 - data AlgDataStruct = Constructor1 Type11 Type12 --always start with capital letter for data
 -                    = Constructor2 Type21 Type22 Type23
 -                    = Constructor3 Type31
 -                    = Constructor4
 -
 - Using pattern-matching for this data (mandatory parenthesis if more than just a single constructor) :
 - foo (Const1 a b) = ...
 - foo (Const2 a b c) = ... 
 - foo (Const3 a) = ... 
 - foo Const4 = ... 
 -}

getName :: Person -> String
getName p@(Person n _ _) = "The name of "++show p++" is "++n

data IntList = Empty
             | Cons Int IntList

myProduct :: IntList -> Int
myProduct Empty       = 1
myProduct (Cons x xs) = x * myProduct xs

myList :: IntList
myList = Cons 7 (Cons 8 (Cons 9 Empty))

--- Homework 2
--

-- This is a seperate file, named LogAnalysisHW2.hs

