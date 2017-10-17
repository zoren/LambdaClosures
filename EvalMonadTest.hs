module EvalMonadTest where

import LambdaCalc
import EvalMonad

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

closureTest =
  lt f (lam x $ lam y $ v x)
  (lt "c1" (app (v f) $ ci 2)
  (app (v "c1") $ ci 3))

getIntEnv var env = getInt(getValueConst(lookupEnv var env))
getConstEnv var env = getValueConst(lookupEnv var env)

ii f c = CInt $ f (getInt c)
iii f c1 c2 = CInt $ f (getInt c1) (getInt c2)

mkClosure f = VClosure (return . VConst . f . getValueConst)
mkClosure2 f = VClosure (\x -> return $ VClosure(\y -> return $ VConst $ f (getValueConst x) (getValueConst y)))

env :: Environment
env = extendEnv "succ" (mkClosure (ii succ))
      (extendEnv "+" (mkClosure2 (iii (+))) emptyEnv)

tests =
  mapM ((\(expected, exp) -> fmap (((==) expected) . getInt . getValueConst) $ run exp env))
  [(2, closureTest)
   ,(1, app (v "succ") $ ci 0)
   ,(3, app (app (v "+") (ci 1)) (ci 2))
   ]
 
