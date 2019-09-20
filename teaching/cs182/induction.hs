
--A tree is either a Nil (zero node tree, aka null)
--or it holds an a along with a left and right subtree
data Tree a = Nil
            | Tree a (Tree a) (Tree a)
            deriving Show

numNodes :: Tree a -> Int
numNodes Nil = 0
numNodes (Tree a left right)
  = 1 + numNodes left + numNodes right

numLeaves :: Tree a -> Int
numLeaves Nil = 0
numLeaves (Tree a Nil Nil) = 1
numLeaves (Tree a left right)
  = numLeaves left + numLeaves right

height :: Tree a -> Int
height Nil = (-1)
height (Tree a left right)
  = 1 + max (height left) (height right)

bigTree =
  Tree 10
      (Tree 5
          (Tree 2 Nil Nil)
          (Tree 4
              (Tree 1 Nil Nil)
              Nil ) )
      (Tree 8
          (Tree 3
              (Tree 1 Nil Nil)
              (Tree 2 Nil Nil) )
          (Tree 3
              Nil
              (Tree 1
                  Nil
                  (Tree 0 Nil Nil) ) ) )
