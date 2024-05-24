{-# LANGUAGE InstanceSigs #-}
module Language.Nano.Unify where

import Language.Nano.Types

infix 6 :~
infix 6 `occursIn`


occursIn :: TyVar -> Type -> Bool
v `occursIn` t = error "TBD: occursIn"


-- | Things that reference type definitions are substitutable:
--   we can replace any reference to a type with its corresponding definition.
class Substitutable a where
  substitute :: (TyVar, Type) -> a -> a

-- | Substitute within a type
instance Substitutable Type where
  substitute :: (TyVar, Type) -> Type -> Type
  substitute def t' = error "TBD: substitute"


-- | A `TypeDefs` is a sequence of type definitions,
--   each of which may be defined in terms of types later in the sequence.
--   An example of a valid `TypeDefs` is
--     [("a", TList (TVar "b")), ("b", TInt)]
--   An example of an *invalid* `TypeDefs is
--     [("b", TInt), ("a", TList (TVar "b"))]
type TypeDefs = [(TyVar, Type)]

substituteAll :: (Substitutable a) => TypeDefs -> a -> a
substituteAll defs t = error "TBD: substituteAll"

-- | A `Constraint` is a pair of `Type`s that we want to "be the same".
--   For instance, `TVar "a" :~ TBool` says that we want `TVar "a"`, a type variable,
--   to be the same as `TBool`: using type `a` should be the same as using type `Bool`.
--   Not all constraints make sense! For instance, `TInt :~ TBool` says that we
--   want to treat `TInt` and `TBool` the same, which is nonsense: we can do things
--   with integers that we can't do with booleans!
data Constraint = Type :~ Type
  deriving (Show)

instance Substitutable Constraint where
  substitute :: (TyVar, Type) -> Constraint -> Constraint
  substitute s (a :~ b) = substitute s a :~ substitute s b

instance Substitutable [Constraint] where
  substitute :: (TyVar, Type) -> [Constraint] -> [Constraint]
  substitute s = map (substitute s)


solve :: [Constraint] -> Either Error TypeDefs
solve cs = unify [] cs
  where
    unify :: TypeDefs -> [Constraint] -> Either Error TypeDefs
    unify defs sys = error "TBD: unify"

    unifyFail :: Type -> Type -> Either Error TypeDefs
    unifyFail a b =
      Left (Error ("cannot unify `" ++ show a ++ "` and `" ++ show b ++ "`"))
