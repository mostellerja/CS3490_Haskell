i :: a -> a
i x = x


k :: a -> b -> a
k x y = x

--'s' is undefined in Haskell
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

w :: a (a-> a => b) -> a -> b
w x y = x y y

b :: (b-> c) -> (a -> b) -> (a -> c)
b x y z = x (y z)

c :: (a -> b -> c) -> (b -> a -> c)
c x y z = x z y


ap :: (a -> b) -> a -> b
--ap x y = x y
ap = ($)

apTest :: [Integer -> Integer]
apTest = [((\x -> x+1), (\x-> x+10), (\x-> (0-x)), (\x -> x^2))


applyFuns :: [a -> b] -> a -> [b]
applyFuns [] x = []
applyFuns (f : fs) x = f x : applyFuns fs x


---------
-- Identity
-- I x = x
i :: a -> a
i = id
-- i x = x
-- i = \x -> x

-- Constant
k :: a -> b -> a
k = const
-- k x y = x
-- k = \x y -> x

-- S-combinator
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)
-- s = \x y z -> x z (y z)

-- Composition
b :: (b -> c) -> (a -> b) -> (a -> c)
b = (.)
-- b x y z = x (y z)
-- b = \x y z -> x (y z)

-- Exchange (flipping the order of arguments in a binary function)
c :: (a -> b -> c) -> b -> a -> c
c = flip
-- c x y z = x z y
-- c = \x y z -> x z y

-- 1 x y = x y is application
ap :: (a -> b) -> (a -> b)
ap = ($)
-- ap x y = x y
-- ap = \x y -> x y
-- ap = \x y -> x $ y

-- omega x = x x is self-application
-- It cannot be defined in Haskell because it does not have a type.
-- Attempting to type it leads ot an infinite constraint a = a -> b
-- omega :: (a -> b) -> b ?
-- omega x = x x

-- The fixed point combinator can be defined by using recursion
-- y :: (a -> a) -> a
y f = f (y f)

-- Computing the fixed point of a function is the purest form of recursion
-- x = f x
x :: Integer
x = (+ 1) x

-- Example of programming with combinators
apTest :: [Integer -> Integer]
apTest = [(\x -> x+1), (\x-> x*10), (\x-> (0-x)), (\x -> x^2)]

applyFuns :: a -> [a -> b] -> [b]
-- applyFuns x [] = []
-- applyFuns x (f : fs) = f x : applyFuns x fs
applyFuns x = map ($ x)
-- applyFuns x (f : fs) = (....) f  : applyFuns x fs
-- want ::  (\f -> f $ x) f = f x

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs