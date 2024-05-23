module Language.Nano.Unify where

import Language.Nano.Types

infix 6 :~


freeTVars :: Type -> [String]
freeTVars t = error "TBD: freeTVars"

-- | Things that reference types in a type environment are substitutable:
--   we can replace any reference to a type for its corresponding definition.
class Substitutable a where
  substitute :: (String, Type) -> a -> a

-- | Substitute within a type
instance Substitutable Type where
  substitute (v, t) t' = error "TBD: substitute"


-- | A type environment is a sequence of type definitions,
--   each of which may be defined in terms of earlier types.
type TypeEnv = [(String, Type)]

substituteAll :: (Substitutable a) => TypeEnv -> a -> a
substituteAll env t = error "TBD: substituteAll"


data Constraint = Type :~ Type
  deriving (Show)

instance Substitutable Constraint where
  substitute s (a :~ b) = substitute s a :~ substitute s b

instance Substitutable [Constraint] where
  substitute = map . substitute


solve :: [Constraint] -> Either Error TypeEnv
solve cs = unify [] cs
  where
    unify :: TypeEnv -> [Constraint] -> Either Error TypeEnv
    unify env sys = error "TBD: unify"

    unifyFail :: Type -> Type -> Either Error TypeEnv
    unifyFail a b =
      Left . Error $
        "cannot unify `" ++ show a ++ "` and `" ++ show b ++ "`"

-- TODO: Test that a `TypeEnv` is applied in the right order.
--   substituteAll [("a", TList (TVar "b")), ("b", TInt)] (TVar "a") == TInt

-- TODO: Test that the `TypeEnv` is built up in the correct order.
--   let Right cs = solve [TVar "a" :~ TList (TVar "b"), TVar "b" :~ TInt] in
--   substituteAll cs (TVar "a") == TInt

-- TODO: Consider writing a function `toUnifier :: TypeEnv -> Subst`
-- that takes e.g. `[("a", TList (TVar "b")), ("b", TInt)]`
-- to `[("a", TList TInt), ("b", TInt)]`.
