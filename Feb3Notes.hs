add10 :: [Integer] -> [Integer]
add10 [] = []
add10 (x:xs) = (x+10) : add10 xs

int2str :: [Integer] -> [String]
int2str [] = []
int2str (x:xs) = (show x) : int2str xs

tagLength :: [String] -> [(String, Int)]
tagLength [] = []
tagLength (x:xs) = (x, length x) : tagLength xs

map' :: (a -> b) -> [a] -> [b]
--map' f [] = []
--map' f (x:xs) = f x : map' f xs
map' f = fold (\x val -> f x : val) []

noThrees :: [Integer] -> [Integer]
noThrees [] = []
noThrees (x:xs) | x/=3 = x : noThrees xs
                | True = noThrees xs
                
removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) | x /= ' ' = x : removeSpaces xs
                    | True     = removeSpaces xs
                    
oddsOnly :: [Integer] -> [Integer]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
                | True = oddsOnly xs
                
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) | p x = x : filter' p xs
                 | True = filter' p xs
                 
--Problem 1
positives :: [Integer] -> [Integer]
positives = filter (>= 0)
--Problem 2
evenLength :: [String] -> [String]
evenLength = filter (even . length)
--Problem 3
doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (x:xs) = (x * 2) : doubleAll xs
--Problem 4
invert :: [Float] -> [Float]
--My Implementation below
invert (x:xs) = (x - (x * 2)): invert xs
--invert (x:xs) = (x * (-1)): invert xs
--Version 2 above
--invert = map (0 -)
--Version 3 above  

len :: [a] -> Integer
--len [] = 0
--len (x:xs) = 1 + len xs
len = fold (\x val -> 1 + val) 0

countInt :: Integer -> [Integer] -> Integer
countInt n [] = 0 
countInt n (x:xs) = if n == x then val  + 1 else val
    where val = countInt n xs

countInt1 :: Integer -> [Integer] -> Integer
countInt1 n = countInt' 0
    where

    
findInt :: Integer -> [Integer] -> Bool
findInt n [] = False
findInt n (x:xs) = if n == x then True else val
    where val = findInt n xs

fold :: (a -> b -> b) -> b -> [a] -> b
fold comb base [] = base
fold comb base (x:xs) = comb x val
    where val = fold comb base xs

--sum :: [Integer] -> Integer
--sum [] = 0
--sum (x:xs) = x + val
--    where val = sum xs
    
sum' :: [Integer] -> Integer
sum' = fold (\x val -> x + val) 0


rev :: [a] -> [a]
rev = rev' []
    where rev' acc [] = acc
          rev' acc (x:xs) = rev' (x:acc) xs
          
rev2 :: [a] -> [a] 
rev2 = foldl (\acc x -> x:acc) []