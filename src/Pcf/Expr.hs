module Pcf.Expr where

import Pcf.Prelude

data Expr
  = Var Text
  | Lam Text Type Expr -- ^ Eg @\a : Bool. a@
  | App Expr Expr -- ^ Eg @a b@

  | Let Text Expr Expr -- ^ Eg @let a = 1 in a@
  | Fix Expr -- ^ Eg @fix (\rec : Nat -> Nat -> Nat. <body-of-factorial>)@
             --
             -- Unlike 'Suc', 'Pred', and 'IsZero' we require an 'Expr'
             -- argument to @Fix@. Otherwise a binding like
             -- @let f = fix in@ wouldn't be explicitly typed.

  | BoolLit Bool
  | IfThenElse Expr Expr Expr -- ^ Eg @if a then b else c@

  | NatLit Natural
  | Suc
  | Pred
  | IsZero
  deriving (Eq, Show)

data Type
  = ArrowType Type Type
  | BoolType
  | NatType
  deriving (Eq, Ord, Show)
