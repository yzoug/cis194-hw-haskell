{-# OPTIONS_GHC -Wall #-}
module LogAnalysisHW2 where

import LogHW2

{- sample of errorHW2.log:
I 6 Completed armadillo processing
I 1 Nothing to report
I 4 Everything normal
I 11 Initiating self-destruct sequence
E 70 3 Way too many pickles
E 65 8 Bad pickle-flange interaction detected
W 5 Flange is due for a check-up
I 7 Out for lunch, back in two time steps
E 20 2 Too many pickles
I 9 Back from lunch
E 99 10 Flange failed!
-}

-- Ex 1
parseMessage :: String -> LogMessage
parseMessage a = parseMessage' (words a) 
    where isInt :: String -> Bool
          isInt []     = True
          isInt (x:xs) = isDigit x && isInt xs
              where isDigit :: Char -> Bool 
                    isDigit num = num `elem` ['0','1'..'9']
          parseMessage' :: [String] -> LogMessage
          parseMessage' []             = Unknown []
          parseMessage' msg@(_:[])      = Unknown (unwords msg)
          parseMessage' msgbis@(_:_:[]) = Unknown (unwords msgbis)
          parseMessage' s@(code:nbr:nbrOrRemaining:remaining) = case code of
                                                          -- In all the following, isInt is necessary so that something like "I love ponies" isn't parsed as an Info
                                                          "I" -> if isInt nbr then LogMessage Info (read nbr::Int) (unwords (nbrOrRemaining:remaining)) else Unknown (unwords s)
                                                          "W" -> if isInt nbr then LogMessage Warning (read nbr) (unwords (nbrOrRemaining:remaining)) else Unknown (unwords s)
                                                          "E" -> if (isInt nbr && isInt nbrOrRemaining) then LogMessage (Error (read nbr)) (read nbrOrRemaining) (unwords remaining) else Unknown (unwords s)
                                                          _   -> Unknown (unwords s) 

parse :: String -> [LogMessage]
parse []   = []
parse file = let (line:remaining) = lines file in
             ((parseMessage line):(parse (unlines remaining)))

-- Ex 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf         = Node Leaf msg Leaf
insert msg@(LogMessage _ time _) (Node treeBelow mid@(LogMessage _ t _) treeAbove)
                        = if time < t then Node (insert msg treeBelow) mid treeAbove else Node treeBelow mid (insert msg treeAbove) 
insert _ _              = undefined

-- Ex 3
build :: [LogMessage] -> MessageTree
build file = buildWithTree file Leaf
    where buildWithTree :: [LogMessage] -> MessageTree -> MessageTree
          buildWithTree [] tree     = tree
          buildWithTree (x:xs) tree = buildWithTree xs (insert x tree)

-- Ex 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                       = []
inOrder (Node lowTree msg topTree) = (inOrder lowTree) ++ [msg] ++ (inOrder topTree)

-- Ex 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = [ c | (LogMessage (Error a) _ c) <- quicksort xs , a >= 50 ] 
    where quicksort :: [LogMessage] -> [LogMessage]
          quicksort []                            = []
          quicksort ((Unknown _):list)            = quicksort list
          quicksort (x@(LogMessage _ t _):remain) = let smallerSorted = quicksort [ (LogMessage a tBis c) | (LogMessage a tBis c) <- remain, tBis <= t ]
                                                        biggerSorted  = quicksort [ (LogMessage a tBis c) | (LogMessage a tBis c) <- remain, tBis > t ] 
                                                    in  smallerSorted ++ [x] ++ biggerSorted

-- Ex 6
-- When using the sample file, my whatWentWrong f'n works very well and returns what needs to be returned. But on the errorHW2.log it just doesn't work and the pc overheats so I can't let it work on it. Guess I'll never figure out how is the hacker in question.

