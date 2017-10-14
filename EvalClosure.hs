module EvalClosure where

import LambdaCalc
import Data.Maybe

data Value variable =
    VConst Const
  | VClosure variable (Environment variable -> Value variable) (Environment variable)
type Environment variable = [(variable, Value variable)]

emptyEnv :: Environment variable
emptyEnv = []

lookupEnv :: (Eq variable) => variable -> Environment variable -> Value variable
lookupEnv var env = fromJust $ lookup var env

extendEnv :: variable -> Value variable -> Environment variable -> Environment variable
extendEnv var exp env = (var, exp) : env

evalExp :: (Eq variable) => Exp variable -> Environment variable -> Value variable
evalExp exp =
  case exp of
    EConst c -> const $ VConst c
    EVar var -> lookupEnv var
    ELambda var body ->
      let cBody = evalExp body in
      \env -> VClosure var cBody env
    EApply e1 e2 ->
      let (c1, c2) = (evalExp e1, evalExp e2)
      in \env ->
           case c1 env of
             VClosure v cBody cloEnv -> cBody (extendEnv v (c2 env) cloEnv)
             _ -> error "applying non-closure"

