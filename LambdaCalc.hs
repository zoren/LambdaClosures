module LambdaCalc where

data Const =
  CInt Int
  | CString String
  deriving (Show, Eq)

type Variable = String

data Exp =
  EConst Const
  | EVar Variable
  | ELambda Variable Exp
  | EApply Exp Exp
