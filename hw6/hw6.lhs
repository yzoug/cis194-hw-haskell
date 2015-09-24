Course
===========

First of all little reminder: how to implement foldl with foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc = foldr (flip f) acc . reverse

Now, I first experiment with the article suggested, and to do so I have to hide the foldr/foldl of Prelude

import Prelude hiding (foldr,foldl)
veryBigList = [1..1000000]

k nevermind I won't experiment with shit

HW
===========
see Fibonnaci.hs


