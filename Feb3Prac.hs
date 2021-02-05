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