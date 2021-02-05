doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = (if x > 100 then x else x*2) + 1
conanO'Brien = "It's a-me, Conan O'Brien!"

fact :: Integer -> Integer
fact  0 = 1
fact n = n * (fact (n-1))

myval :: Integer
myval = 
    let x = fact 200
        y = x + x
    in x + y
    
myval' :: Integer
myval' = (fact 200) + (fact 200 + fact 200)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib2 :: (Integer,Integer) -> (Integer,Integer)
fib2 (0,_) = (1,0)
fib2 (m,n) = (m+n,m)