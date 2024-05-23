module Language.Nano.TypeCheck where

import Prelude hiding (and)

import Language.Nano.Types
import Language.Nano.Parser (parseFile, parseString)
import Language.Nano.Unify (Constraint(..), solve, substituteAll)

infixr 3 `and`
infix  4 ~


data FreshNames = Next Type FreshNames
type Fresh a = FreshNames -> (a, FreshNames)

exists :: (Type -> Fresh [Constraint]) -> Fresh [Constraint]
exists f freshes = error "TBD: exists"

(~) :: Type -> Type -> Fresh [Constraint]
(a ~ b) freshes = error "TBD: (~)"

and :: Fresh [Constraint] -> Fresh [Constraint] -> Fresh [Constraint]
(fc1 `and` fc2) freshes = error "TBD: and"


data Binding
  = BoundVar Type
  | BoundLet Expr BindEnv
  | BoundPrim (Type -> Fresh [Constraint])

-- | Binding environment: maps variables to their binding information.
type BindEnv = [(Id, Binding)]

-- | Types of built-in operators and functions
preludeTypes :: BindEnv
preludeTypes =
  -- "+" can be used at a type `t` if `t` is exactly `TInt :=> TInt :=> TInt`.
  [ ("+",    BoundPrim (\t -> t ~ TInt :=> TInt :=> TInt))
  , ("-",    error "TBD: (-)")
  , ("*",    error "TBD: (*)")
  , ("/",    error "TBD: (/)")
  , ("<",    error "TBD: (<)")
  , ("<=",   error "TBD: (<=)")
  , ("&&",   error "TBD: (&&)")
  , ("||",   error "TBD: (||)")
  -- "==" can be used at a type `t` if there exists some type `a` such
  -- that `t` is exactly `a :=> a :=> TBool`.
  , ("==",   BoundPrim (\t -> exists (\a -> t ~ a :=> a :=> TBool)))
  , ("!=",   error "TBD: (!=)")
  , ("if",   error "TBD: if")
  , ("[]",   error "TBD: []")
  , (":",    error "TBD: (:)")
  , ("head", error "TBD: head")
  , ("tail", error "TBD: tail")
  ]

lookupBinding :: Id -> BindEnv -> (Id, Binding)
lookupBinding v [] = error ("unbound variable `" ++ v ++ "`")
lookupBinding v ((v', b) : bs)
  | v == v'   = (v', b)
  | otherwise = lookupBinding v bs

-- | The invocation `check e env t` asks the question,
-- | "What must hold true for `e` to be used at type `t` in environment `env`?"
check :: Expr -> BindEnv -> Type -> Fresh [Constraint]
check (EInt _) env t =
  error "TBD: check EInt"
check (EBool _) _env t =
  error "TBD: check EBool"
check (EVar v) env t =
  checkBound (lookupBinding v env) t
  where
    checkBound :: (Id, Binding) -> Type -> Fresh [Constraint]
    checkBound (v, b) t = error "TBD: checkBound"
check (ELet v b a) env t =
  error "TBD: check ELet"
check (ELam v b) env t =
  error "TBD: check ELam"
check (EApp f a) env t =
  error "TBD: check EApp"
check (EBin op a b) env t =
  check (EVar (show op) `appMany` [a, b]) env t
check (EIf cond a b) env t =
  check (EVar "if" `appMany` [cond, a, b]) env t
check ENil env t =
  check (EVar "[]") env t


--------------------------------------------------------------------------------
typeOfFile :: FilePath -> IO (Either Error Type)
typeOfFile f = parseFile f >>= (return . typeOfExpr)

typeOfString :: String -> Either Error Type
typeOfString s = typeOfExpr (parseString s)

typeOfExpr :: Expr -> Either Error Type
typeOfExpr expr =
  let Next ttop freshes = freshNames 0 in
  let (cs, _) = check expr preludeTypes ttop freshes in
  either Left (Right . flip substituteAll ttop) (solve cs)

freshNames :: Int -> FreshNames
freshNames n = Next (TVar ("a" ++ show n)) (freshNames (n + 1))
