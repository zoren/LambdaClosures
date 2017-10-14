module LambdaCalc where

data Const =
  CInt Int
  | CString String
  deriving (Show, Eq)

data Exp variable =
  EConst Const
  | EVar variable
  | ELambda variable (Exp variable)
  | EApply (Exp variable) (Exp variable)
