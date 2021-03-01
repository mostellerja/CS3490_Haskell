--March 1st Notes


-- needed for nub :: (Eq a) => [a] -> [a], removeDups from the standard library
import Data.List

-- Example 1: Encoding Propositional Formulas into Haskell

type Vars = String
-- type Vars = Integer

data Prop = Var Vars | And Prop Prop | Or Prop Prop | Imp Prop Prop
                     | Not Prop | Iff Prop Prop | TT | FF
  deriving Show

-- A /\ B /\ C -> B \/ D

data Comb = I | K | S | App Comb Comb

-- S(KI)IS = ((S(KI))I)S
comb1 :: Comb
comb1 = App (App (App S (App K I)) I) S

-- K(KI)SIS = (((K(KI))S)I)S
comb2 :: Comb
comb2 = App (App (App (App K (App K I)) S) I) S


-- Example 4: Encoding Lambda Terms into Haskell

data Lambda = LVar Vars | LApp Lambda Lambda | LAbs Vars Lambda

-- \x.x(\y.yx)
lam1 :: Lambda
lam1 = LAbs "X" (LApp (LVar "X")
                      (LAbs "Y" (LApp (LVar "Y") (LVar "X"))))

-- \x.x(\y.yz)
lam2 :: Lambda
lam2 = LAbs "X" (LApp (LVar "X")
                      (LAbs "Y" (LApp (LVar "Y") (LVar "Z"))))

-- (\x.x(\y.yz))y
lam3 :: Lambda
lam3 = LApp (LAbs "X" (LApp (LVar "X")
                      (LAbs "Y" (LApp (LVar "Y") (LVar "Z")))))
            (LVar "Y")

-- Function computes the set of *free* variables in a lambda term
-- IE, those variables that occur outside the scope of lambda.
lfv :: Lambda -> [Vars]
lfv (LVar x) = [x]
lfv (LApp t1 t2) = nub (lfv t1 ++ lfv t2)
lfv (LAbs x t) = filter (/= x) $ lfv t