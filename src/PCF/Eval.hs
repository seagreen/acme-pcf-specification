module PCF.Eval where

import PCF.Expr
import PCF.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

-- * TermEnv

newtype TermEnv
  = TermEnv (HashMap Text Value)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

lookup :: Text -> TermEnv -> Maybe Value
lookup id (TermEnv env) =
  HashMap.lookup id env

insert :: Text -> Value -> TermEnv -> TermEnv
insert id expr (TermEnv env) =
  TermEnv (HashMap.insert id expr env)

-- * Value

data Value
  = LamVal TermEnv Text Expr
  | BoolVal Bool
  | NatVal Natural
  | SucVal
  | PredVal
  | IsZeroVal
  deriving (Eq, Show)

prettyValue :: Value -> Text
prettyValue = \case
  BoolVal b ->
    if b then "true" else "false"

  NatVal n ->
    Text.pack (show n)

  other ->
    Text.pack (show other)

-- * Eval

data Error
  = NotInScope Text
  | NotNat Value
  | AppliedNonFunction Value
  | FixNotLambda Value
  | IfNotBool Value
  deriving (Eq, Show)

eval :: TermEnv -> Expr -> Either Error Value
eval env = \case
  Var id ->
    case lookup id env of
      Nothing ->
        Left (NotInScope id)

      Just val ->
        Right val

  Lam id _typ expr ->
    Right (LamVal env id expr)

  App e1 e2 -> do
    v1 <- eval env e1
    v2 <- eval env e2 -- TODO: evaluation order?

    let
      assertNat :: Value -> Either Error Natural
      assertNat = \case
        NatVal n ->
          Right n

        other ->
          Left (NotNat other)

    case v1 of
      LamVal closure id body ->
        eval (insert id v2 closure) body

      SucVal -> do
        n <- assertNat v2
        Right (NatVal (n + 1))

      PredVal -> do
        n <- assertNat v2
        if n == 0
          then
            Right (NatVal 0)

          else
            Right (NatVal (n - 1))

      IsZeroVal -> do
        n <- assertNat v2
        Right (BoolVal (n == 0))

      _ ->
        Left (AppliedNonFunction v1)

  Let id e1 e2 -> do
    v1 <- eval env e1
    eval (insert id v1 env) e2

  Fix expr -> do
    -- Example expr:
    --
    --     \rec : Nat -> Nat. \x : Nat. if is-zero x then 0 else rec (pred x)

    v1 <- eval env expr
    case v1 of

      -- Example recId:
      --
      --     rec : Nat -> Nat
      --
      -- Example body:
      --
      --     \x : Nat. if is-zero x then 0 else rec (pred x)
      LamVal closure _recId body ->
        case body of

          -- Example id:
          --
          --     x : Nat
          Lam id _typ _ ->
            Right (LamVal closure id
                    (App
                      (App expr (Fix expr))
                      (Var id)))

          -- Example expression that hits this case:
          --
          --     fix (\x : Nat. 1)
          _ ->
            eval env body

      _ ->
        Left (FixNotLambda v1)

  BoolLit b ->
    Right (BoolVal b)

  IfThenElse bExpr e1 e2 -> do
    bVal <- eval env bExpr
    case bVal of
      BoolVal True ->
        eval env e1

      BoolVal False ->
        eval env e2

      _ ->
        Left (IfNotBool bVal)

  NatLit n ->
    Right (NatVal n)

  Suc ->
    Right SucVal

  Pred ->
    Right PredVal

  IsZero ->
    Right IsZeroVal
