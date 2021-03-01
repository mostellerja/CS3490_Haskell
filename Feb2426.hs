-- needed for nub :: (Eq a) => [a] -> [a], removeDups from the standard library
import Data.List

-- Example 1: Encoding Propositional Formulas into Haskell

type Vars = String
-- type Vars = Integer

data Prop = Var Vars | And Prop Prop | Or Prop Prop | Imp Prop Prop
                     | Not Prop | Iff Prop Prop | TT | FF
  deriving Show

-- A /\ B /\ C -> B \/ D
prop1 :: Prop
prop1 = Imp (And (And (Var "A") (Var "B"))
                 (Var "C"))
            (Or  (Var "B") (Var "D"))

prop2 :: Prop
prop2 = Imp (And (And (Var "A") (Var "B"))
                 (Var "C"))
            (And  (Var "B") (Var "D"))

-- If we change type Vars = Integer,
--   the variable leaves will be labelled by integers,
--   but none of the functions below would need to change
-- A /\ B /\ C -> B \/ D
-- X0 /\ X1 /\ X2 -> X1 \/ X3
-- prop3 = Imp (And (And (Var 0) (Var 1))
--                  (Var 2))
--             (Or  (Var 1) (Var 3))


-- Example 2: Encoding Arithmetic Expressions into Haskell

data AExpr = AVar Vars | Add AExpr AExpr | Mul AExpr AExpr | Neg AExpr
                       | Const Integer
                       | Mod AExpr AExpr | Expt AExpr AExpr | Div AExpr AExpr

-- (3x^2-2y)/2 = (3x^2+(-2y))/2
aexpr1 :: AExpr
aexpr1 = Div (Add (Mul (Const 3)
                       (Mul (AVar "X") (AVar "X")))
                  (Neg (Mul (Const 2)  (AVar "Y"))))
             (Const 2)


-- Example 3: Encoding Combinators into Haskell

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


-- Propositions example, continued.

-- data Prop = Var String | And Prop Prop | Or Prop Prop | Imp Prop Prop
--                        | Not Prop | Iff Prop Prop | TT | FF

-- Returns all the variables occurring in the formula, with no repetitions
fv :: Prop -> [Vars]
fv (Var x) = [x]
fv (And f1 f2) = nub $ fv f1 ++ fv f2
fv (Or  f1 f2) = nub $ fv f1 ++ fv f2
fv (Imp f1 f2) = nub (fv f1 ++ fv f2)
fv (Not f)     = fv f
fv (Iff f1 f2) = nub (fv f1 ++ fv f2)
fv (TT) = []
fv (FF) = []

-- depth of a propositional formula
depth :: Prop -> Integer
depth (Var x) = 0
depth TT = 0
depth FF = 0
depth (And p1 p2) = max (depth p1) (depth p2) + 1
depth (Or p1 p2) = max (depth p1) (depth p2) + 1
depth (Imp p1 p2) = max (depth p1) (depth p2) + 1
depth (Iff p1 p2) = max (depth p1) (depth p2) + 1
depth (Not p) = depth p + 1

-- An Boolean environment is a function from Variables to Booleans
type BEnv = Vars -> Bool

-- an environment mapping variables to booleans
env :: BEnv
env "A" = True
env "B" = True
env "C" = True
env "D" = False
env _ = error "No value"

-- given an environment into booleans,
-- any proposition can be evaluated to a boolean
eval :: BEnv -> Prop -> Bool
eval env (Var x) = env x
eval env (TT) = True
eval env (FF) = False
eval env (And p1 p2) = eval env p1 && eval env p2
eval env (Or p1 p2) = eval env p1 || eval env p2
eval env (Imp p1 p2) = if (eval env p1) then (eval env p2) else True
eval env (Iff p1 p2) = eval env p1 == eval env p2
eval env (Not p) = not (eval env p)

-- an environment mapping variables to integers
myEnv :: Vars -> Integer
myEnv "X" = 10
myEnv "Y" = 4
myEnv _ = error "no binding"

-- given an environment into integers,
-- any arithmetic expression can be evaluated to an integer
aeval :: (Vars -> Integer) -> AExpr -> Integer
aeval env (AVar x) = env x
aeval env (Add e1 e2) = aeval env e1 + aeval env e2
aeval env (Mul e1 e2) = aeval env e1 * aeval env e2
aeval env (Neg e) = - (aeval env e)
aeval env (Const c) =  c
aeval env (Mod e1 e2) = aeval env e1 `mod` aeval env e2
aeval env (Expt e1 e2) = aeval env e1 ^ aeval env e2
aeval env (Div e1 e2) = aeval env e1 `div` aeval env e2