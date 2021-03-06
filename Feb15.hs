-- Haskell's builtin List datatype can be thought of as generated by
--
--    [a]  ::=   []    |    a : [a]
--
-- eg, [1,2,3] = 1 : (2 : (3 : []))

-- Here is the definition of a NEW type, called List,
-- which is equivalent to Haskell's built-in type above
data List a = Nil | Cons a (List a)
  deriving (Show,Eq,Ord)

-- New datatype X being defined by "data X"
-- On the right side, give *constructors*: Leaf and Node
-- They an be used as functions:
--
-- *Main> :t Nil
-- Nil :: List a
-- *Main> :t Cons
-- Cons :: a -> List a -> List a
--
-- What makes them special, is that they can also be used in pattern-matching

-- [1,2,3]
test1 :: List Integer
test1 = Cons 1 (Cons 2 (Cons 3 Nil))

-- Converting from [a] to List
hs2list :: [a] -> List a
hs2list [] = Nil
hs2list (x:xs) = Cons x (hs2list xs)
-- and back
list2hs :: List a -> [a]
list2hs Nil = []
list2hs (Cons x xs) = x : list2hs xs

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show,Eq)

-- This function creates a node with empty subtrees
leafNode :: a -> Tree a
leafNode x = Node x Leaf Leaf


-- Examples of trees
tree :: Tree Integer
tree = Node 4 (Node 2 (Node (-5) Leaf Leaf)
                      (Node 0    Leaf Leaf) )
              (Node 3 Leaf Leaf)

tree1 :: Tree Integer
tree1 = Node 21 (Node (-1) (Node (-5) Leaf Leaf)
                      (Node 9    Leaf Leaf) )
              (Node 3 Leaf Leaf)

test2 :: Tree Integer
test2 = Node 3 (Node 2 (Node 1 Leaf Leaf)
                      (Node 0 Leaf Leaf))
              (Node 4 Leaf Leaf)

test2' :: Tree Integer
test2' = Node 3 (Node 2 (leafNode 1) (leafNode 0))
               (leafNode 4)

test2'' :: Tree Integer
test2'' = Node 3 (Node (-2) (leafNode 1) (leafNode 0))
                 (leafNode 4)

tree2 :: Tree Double
tree2 = Node pi (Node 10 Leaf Leaf) (Node (-100) Leaf Leaf)

tree3 :: Tree String
tree3 = Node "The" (leafNode "Jack") (Node "Dog" Leaf (Node "" Leaf Leaf))

tree4 :: Tree String
tree4 = Node "The" (leafNode "Jack") (Node "Dog" Leaf Leaf)

test4 :: Tree String
test4 = Node "Bit" (Node "Angry"
                         (leafNode "The")
                         (leafNode "Dog"))
                   (Node "Gray"
                         (leafNode "The")
                         (leafNode "Cat"))

-- Functions defined on trees by recursion
sumTree :: Tree Integer -> Integer
sumTree Leaf = 0
sumTree (Node x t1 t2) = x + sumTree t1 + sumTree t2

multTree :: Tree Double -> Double
multTree Leaf = 1
multTree (Node x t1 t2) = x * multTree t1 * multTree t2

concatTree :: Tree String -> String
concatTree Leaf = "" -- [] for strings
concatTree (Node s t1 t2) = concatTree t1 ++ s ++ concatTree t2

anyEven :: Tree Integer -> Bool
anyEven Leaf = False
anyEven (Node x t1 t2) = even x || anyEven t1 || anyEven t2

anyNull :: Tree String -> Bool
anyNull Leaf = False
anyNull (Node x t1 t2) = null x || anyNull t1 || anyNull t2

anyTree :: (a -> Bool) -> Tree a -> Bool
anyTree p Leaf = False
anyTree p (Node x t1 t2) = p x || anyTree p t1 || anyTree p t2

-- function returns true if every descendant of a node is less than, or equals it
checkOrder :: Tree Integer -> Bool
checkOrder Leaf = True
checkOrder (Node x t1 t2) = checkOrder' x t1 && checkOrder' x t2
  where checkOrder' y Leaf = True
        checkOrder' y (Node z u1 u2) = z <= y && checkOrder' z u1 && checkOrder' z u2

-- When there is no data, return Nothing
-- When there is data, return Just (the maximum element)
maxTree :: Tree Integer -> Maybe Integer
maxTree Leaf = Nothing
maxTree (Node x t1 t2) =
  let r1 = maxTree t1
      r2 = maxTree t2
    in case (r1,r2) of
      (Nothing,  Nothing) -> Just x
      (Just y ,  Nothing) -> if x<y then Just y else Just x
      (Nothing,  Just z ) -> if x<z then Just z else Just x
      (Just y ,  Just z ) -> Just (maximum [x,y,z])

-- Compare:
-- data Either a b = Left a | Right b
-- data Maybe a = Nothing | Just a
-- data List a = Nil | Cons a (List a)
-- data Tree a = Leaf | Node a (Tree a) (Tree a)
data Nat = Zero | Succ Nat

natThree :: Nat
natThree = Succ (Succ (Succ Zero))

-- Can you implement the following function?
-- mapTree :: (a -> b) -> Tree a -> Tree b