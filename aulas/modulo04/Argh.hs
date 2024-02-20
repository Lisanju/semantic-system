module Argh where 

import Data.List

recognize :: String -> Bool
recognize []       = False
recognize s@(x:xs) = (s == "argh!") 
                  || (x == 'a' && recognize xs)


string2length :: Tree String -> Tree Int
string2length (Leaf s)    = Leaf (length s)
string2length (Branch ts) = Branch (map string2length ts)


generate :: [String]
generate = ["argh!"] ++ map ("a" ++) generate

data Tree a = Leaf a | Branch [Tree a] deriving (Eq,Show)

--instance (Show a) => Show (Tree a) where 
--  show (Leaf a)    = show a
--  show (Branch ts) = "[" ++ show ts ++ "]"

parse :: String -> [Tree String]
parse []       = []
parse s@(x:xs) = [Leaf   "argh!"      | s == "argh!"]
              ++ [Branch [Leaf "a",t] | t <- parse xs,
                                        x == 'a']
