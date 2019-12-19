module PCF.Eval where

import PCF.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified PCF.Expr as Typed

-- * TermEnv

newtype TermEnv
  = TermEnv (HashMap Text Expr)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

lookup :: Text -> TermEnv -> Maybe Expr
lookup id (TermEnv env) =
  HashMap.lookup id env

insert :: Text -> Expr -> TermEnv -> TermEnv
insert id expr (TermEnv env) =
  TermEnv (HashMap.insert id expr env)

-- * Untyped expr

data Expr
  = Var Text
  | Lam (Maybe TermEnv) Text Expr
  | App Expr Expr

  | Let Text Expr Expr
  | Fix Expr

  | BoolLit Bool
  | IfThenElse Expr Expr Expr

  | NatLit Natural
  | Suc
  | Pred
  | IsZero
  deriving (Eq, Show)

erase :: Typed.Expr -> Expr
erase = \case
  Typed.Var id ->
    Var id

  Typed.Lam id _typ expr ->
    Lam Nothing id (erase expr)

  Typed.App e1 e2 ->
    App (erase e1) (erase e2)

  Typed.Let id e1 e2 ->
    Let id (erase e1) (erase e2)

  Typed.Fix expr ->
    Fix (erase expr)

  Typed.BoolLit b ->
    BoolLit b

  Typed.IfThenElse e1 e2 e3 ->
    IfThenElse (erase e1) (erase e2) (erase e3)

  Typed.NatLit n ->
    NatLit n

  Typed.Suc ->
    Suc

  Typed.Pred ->
    Pred

  Typed.IsZero ->
    IsZero

-- * Eval

data Error
  = NotInScope Text
  | NotNat Expr
  | AppliedNonFunction Expr
  | FixNotLambda Expr
  | IfNotBool Expr
  deriving (Eq, Show)

eval :: TermEnv -> Expr -> Either Error Expr
eval env = \case
  Var id ->
    case lookup id env of
      Nothing ->
        Left (NotInScope id)

      Just expr ->
        Right expr

  Lam mClosure id expr ->
    case mClosure of
      Nothing ->
        Right (Lam (Just env) id expr)

      Just _ ->
        Right (Lam mClosure id expr)

  App e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2 -- TODO: evaluation order?

    let
      assertNat :: Expr -> Either Error Natural
      assertNat = \case
        NatLit n ->
          Right n

        other ->
          Left (NotNat other)

    case v1 of
      Lam mClosure id body -> do
        let lEnv = case mClosure of
                     Nothing ->
                       env

                     Just closure ->
                       closure

        eval (insert id v2 lEnv) body

      Suc -> do
        n <- assertNat v2
        Right (NatLit (n + 1))

      Pred -> do
        n <- assertNat v2
        if n == 0
          then
            Right (NatLit 0)

          else
            Right (NatLit (n - 1))

      IsZero -> do
        n <- assertNat v2
        Right (BoolLit (n == 0))

      _ ->
        Left (AppliedNonFunction v1)

  Let id e1 e2 -> do
    v1 <- eval env e1
    eval (insert id v1 env) e2

  Fix expr -> do
    v1 <- eval env expr

    case v1 of
      Lam (Just closure) id body ->
        eval (insert id body closure) body

      _ ->
        Left (FixNotLambda v1)

  BoolLit b ->
    Right (BoolLit b)

  IfThenElse bExpr e1 e2 -> do
    bVal <- eval env bExpr
    case bVal of
      BoolLit True ->
        eval env e1

      BoolLit False ->
        eval env e2

      _ ->
        Left (IfNotBool bVal)

  NatLit n ->
    Right (NatLit n)

  Suc ->
    Right Suc

  Pred ->
    Right Pred

  IsZero ->
    Right IsZero
