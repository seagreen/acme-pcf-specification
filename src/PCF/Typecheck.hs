module PCF.Typecheck where

import PCF.Expr
import PCF.Prelude

import qualified Data.HashMap.Strict as HashMap

-- * TypeEnv

newtype TypeEnv
  = TypeEnv (HashMap Text Type)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

lookup :: Text -> TypeEnv -> Maybe Type
lookup id (TypeEnv env) =
  HashMap.lookup id env

insert :: Text -> Type -> TypeEnv -> TypeEnv
insert id expr (TypeEnv env) =
  TypeEnv (HashMap.insert id expr env)

-- * Typecheck

data Error
  = NotInScope Text
  | AppNotFunction Type
  | AppliedWrongType
      Type -- ^ expected
      Type -- ^ actual
  | FixNotLambda Type
  | FixTypeMismatch Type Type
  | IfNotBool Type
  | IfBranchesMismatch Type Type
  deriving (Eq, Show)

typecheck :: TypeEnv -> Expr -> Either Error Type
typecheck env = \case
  Var id ->
    case lookup id env of
      Nothing ->
        Left (NotInScope id)

      Just typ ->
        Right typ

  Lam id argType body -> do
    bodyType <- typecheck (insert id argType env) body
    pure (ArrowType argType bodyType)

  App e1 e2 -> do
    t1 <- typecheck env e1
    t2 <- typecheck env e2
    case t1 of
      ArrowType argType outputType -> do
        when (t2 /= argType) (Left (AppliedWrongType argType t2))
        Right outputType

      _ ->
        Left (AppNotFunction t1)

  Let id e1 e2 -> do
    t1 <- typecheck env e1
    typecheck (insert id t1 env) e2

  Fix expr -> do
    typ <- typecheck env expr
    case typ of
      ArrowType argType outputType -> do
        when (argType /= outputType) (Left (FixTypeMismatch argType outputType))
        Right outputType

      _ ->
        Left (FixNotLambda typ)

  BoolLit _ ->
    Right BoolType

  IfThenElse e1 e2 e3 -> do
    t1 <- typecheck env e1
    when (t1 /= BoolType) (Left (IfNotBool t1))
    t2 <- typecheck env e2
    t3 <- typecheck env e3
    when (t2 /= t3) (Left (IfBranchesMismatch t2 t3))
    Right t2

  NatLit _ ->
    Right NatType

  Suc ->
    Right (ArrowType NatType NatType)

  Pred ->
    Right (ArrowType NatType NatType)

  IsZero ->
    Right (ArrowType NatType BoolType)
