data Tree a = Leaf a| Branch (Tree a) a (Tree a) deriving (Eq, Show)
x = Branch (Branch (Leaf 2) 4 (Leaf 5)) 10 (Leaf 1)
instance Functor Tree where 
    fmap f (Leaf a) = Leaf (f a) 
    fmap f (Branch x y z) = Branch (fmap f x) (f y) (fmap f z)