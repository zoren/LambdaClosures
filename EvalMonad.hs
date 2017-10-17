module EvalMonad where

import LambdaCalc

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

data Value =
    VConst Const
  | VClosure (Value -> Evaluation Value)

instance Show Value where
  show (VConst c) = show c
  show (VClosure _) = "<closure>"

type Environment = Map Variable Value

type Evaluation a = State Environment a

emptyEnv :: Environment
emptyEnv = Map.empty

lookupEnv :: Variable -> Environment -> Value
lookupEnv var env = env Map.! var

extendEnv :: Variable -> Value -> Environment -> Environment
extendEnv var value env = Map.insert var value env

applyFunc :: Value -> Value -> Evaluation Value
applyFunc (VClosure f) arg = f arg
applyFunc _ _ = error "applying non-closure"

evalExp :: Exp -> Evaluation Value
evalExp exp =
  case exp of
    EConst c -> return $ VConst c
    EVar var -> lookupEnv var <$> get
    ELambda var body ->
      let cBody = evalExp body
      in return $ VClosure (\value -> withStateT (extendEnv var value) cBody)
    EApply e1 e2 ->
      do
        v1 <- evalExp e1
        v2 <- evalExp e2
        applyFunc v1 v2

run exp env = evalStateT (evalExp exp) env
