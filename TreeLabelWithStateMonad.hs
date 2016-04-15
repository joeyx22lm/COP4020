module TreeLabelWithStateMonad where

import Store
import Control.Monad.State

-- label element

justToInt Nothing = 99999999
justToInt (Just a) = a

labelValue :: Ord a => a -> State (Store a Int) Int
labelValue val = do
    (Store sto) <- get
    if (lookupStore val (Store sto)) == Nothing
    then do
        let newLabel = createNewLabel (Store sto)
        put (insertStore val newLabel (Store sto))
        return newLabel
    else do
        return (justToInt (lookupStore val (Store sto)))

-- getLabeledTree (Node "Haskell" (Node "JoeyRox" Nil (Node "Haskell" Nil Nil)) (Node "HelloWorld" (Node "Hello" Nil (Node "JoeyRox" Nil Nil)) (Node "EndOfTheLine" Nil Nil)))
-- Node 0 (Node 1 Nil (Node 0 Nil Nil)) (Node 2 (Node 3 Nil (Node 1 Nil Nil)) (Node 4 Nil Nil))

-- label tree

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Eq)

labelTree :: Ord a => Tree a -> State (Store a Int) (Tree Int)
labelTree Nil = do
  return Nil
labelTree (Node val left right) = do
  labeledValue <- labelValue val
  labeledLeft  <- labelTree left
  labeledRight <- labelTree right
  return (Node labeledValue labeledLeft labeledRight)

getLabeledTree :: Ord a => Tree a -> Tree Int
getLabeledTree tree = fst $ runState (labelTree tree) (emptyStore)
