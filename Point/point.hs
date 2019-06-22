data Point a = Point a a deriving (Eq, Show)
instance Functor Point where fmap f (Point x y) = Point (f x) (f y)