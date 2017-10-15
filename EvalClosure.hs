module EvalClosure where

import LambdaCalc

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Value variable =
    VConst Const
  | VClosure (Value variable -> Value variable)
type Environment variable = Map variable (Value variable)

emptyEnv :: Environment variable
emptyEnv = Map.empty

lookupEnv :: (Ord variable) => variable -> Environment variable -> Value variable
lookupEnv var env = env Map.! var

extendEnv :: (Ord variable) => variable -> Value variable -> Environment variable -> Environment variable
extendEnv var value env = Map.insert var value env

applyFunc :: (Ord v) => Value v -> Value v -> Value v
applyFunc (VClosure f) arg = f arg
applyFunc _ _ = error "applying non-closure"

evalExp :: (Ord variable) => Exp variable -> Environment variable -> Value variable
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
