curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f a b = f b a
