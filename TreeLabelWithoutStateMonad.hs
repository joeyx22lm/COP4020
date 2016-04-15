module TreeLabelWithoutStateMonad where

import Store

-- label element
justToInt Nothing = 9999
justToInt (Just a) = a

labelValue :: Ord a => a -> (Store a Int) -> (Int, Store a Int)
labelValue val ls =
    let labelVal = if ((lookupStore val ls) == Nothing) then (createNewLabel ls) else (justToInt (lookupStore val ls))
        newStore = (insertStore val labelVal ls)
        in (labelVal, newStore)

-- getLabeledTree (Node "Haskell" (Node "JoeyRox" Nil (Node "Haskell" Nil Nil)) (Node "HelloWorld" (Node "Hello" Nil (Node "JoeyRox" Nil Nil)) (Node "EndOfTheLine" Nil Nil)))
-- Node 0 (Node 1 Nil (Node 0 Nil Nil)) (Node 2 (Node 3 Nil (Node 1 Nil Nil)) (Node 4 Nil Nil))

-- label tree

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Eq)

labelTree :: Ord a => Tree a -> (Store a Int) -> (Tree Int, Store a Int)
labelTree Nil ls = (Nil, ls)

labelTree (Node val left right) ls =
  (Node labeledValue labeledLeft labeledRight, ls''')
    where (labeledValue, ls')   = labelValue val   ls
          (labeledLeft,  ls'')  = labelTree  left  ls'
          (labeledRight, ls''') = labelTree  right ls''

getLabeledTree :: Ord a => Tree a -> Tree Int
getLabeledTree tree = fst $ labelTree tree emptyStore
