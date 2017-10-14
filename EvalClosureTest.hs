module EvalClosureTest where

import LambdaCalc
import EvalClosure

getInt (CInt i) = i

getValueConst (VConst c) = c
getValueConst _ = error "not a constant value"

x = "x"
y = "y"
f = "f"

i = CInt
s = CString

c = EConst
ci = c.i
cs = c.s

v = EVar
lam = ELambda
app = EApply

lt v e1 e2 = app (lam v e2) e1  

e =
  lt f (lam x $ lam y $ v x)
  (lt "c1" (app (v f) $ ci 2)
  (app (v "c1") $ ci 3))

getIntEnv var env = getInt(getValueConst(lookupEnv var env))
getConstEnv var env = getValueConst(lookupEnv var env)

mkIntClosure var f = VClosure var (\env -> f $ getIntEnv var env) emptyEnv

ii f c = CInt $ f (getInt c)
iii f c1 c2 = CInt $ f (getInt c1) (getInt c2)

mkClosure x f = VClosure x (\env -> VConst $ f (getConstEnv x env)) emptyEnv
mkClosure2 x y f =
  let inner envy = VConst $ f (getConstEnv x envy) (getConstEnv y envy)
  in VClosure x (\envx -> VClosure y inner (extendEnv x (lookupEnv x envx) emptyEnv) ) emptyEnv

env = extendEnv "succ" (mkClosure "i" (ii succ))
      (extendEnv "+" (mkClosure2 "x" "y" (iii (+))) emptyEnv)

tests = [i 2 == getValueConst (evalExp e env),
         i 1 == getValueConst (evalExp (app (v "succ") $ ci 0) env),
         i 3 == getValueConst (evalExp (app (app (v "+") (ci 1)) (ci 2)) env) 
        ]
 