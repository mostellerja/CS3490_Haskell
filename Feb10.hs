findString :: Int -> [String] -> String
findString n [] = ""
findString n (x:xs) = if length x == n then x else findString n xs


findString' :: Int -> [String] -> String
findString' n = foldr (\x val -> if length x == n then x else val) ""
--findString' n = foldr

findFun :: String -> String -> String
--findFun x val = if length x == n then x else val
findFun = \x val -> if length x == n  then x else val


