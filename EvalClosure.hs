module EvalClosure where

import LambdaCalc

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Value =
    VConst Const
  | VClosure (Value -> Value)
type Environment = Map Variable Value

emptyEnv :: Environment
emptyEnv = Map.empty

lookupEnv :: Variable -> Environment -> Value
lookupEnv var env = env Map.! var

extendEnv :: Variable -> Value -> Environment -> Environment
extendEnv var value env = Map.insert var value env

applyFunc :: Value -> Value -> Value
applyFunc (VClosure f) arg = f arg
applyFunc _ _ = error "applying non-closure"

evalExp :: Exp -> Environment -> Value
evalExp exp =
  case exp of
    EConst c -> const $ VConst c
    EVar var -> lookupEnv var
    ELambda var body ->
      let cBody = evalExp body in
      \env -> VClosure (\value -> cBody $ extendEnv var value env)
    EApply e1 e2 ->
      let (c1, c2) = (evalExp e1, evalExp e2)
      in \env -> applyFunc (c1 env) (c2 env)
