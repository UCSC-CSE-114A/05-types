import Control.Exception
import Test.Tasty
import Common

import qualified Language.Nano.Types     as Nano
import qualified Language.Nano.Unify     as Nano
import qualified Language.Nano.TypeCheck as Nano

import Data.List (isInfixOf, sort, nub)
import Data.Monoid (All(..), Any(..))
import Language.Nano.Types (Type(..), Error(..))
import Language.Nano.Unify (Constraint(..))

main :: IO ()
main = runTests [ unit ]

unit :: Score -> TestTree
unit sc = testGroup "NANO"
  [ -- 1a tests
    scoreTest ( ("a" `Nano.occursIn`)
              , TVar "a"
              , True
              , 1 -- points for this test case
              , "occursIn 1")
  , scoreTest ( ("a" `Nano.occursIn`)
              , TList (TList (TVar "a"))
              , True
              , 1
              , "occursIn 2")
  , scoreTest ( mconcat (map ((All .) . Nano.occursIn) ["a", "b"])
              , TVar "a" :=> TVar "b"
              , All True
              , 1
              , "occursIn 3")
  , scoreTest ( ("a" `Nano.occursIn`)
              , TVar "a" :=> TVar "a"
              , True
              , 1
              , "occursIn 4")
  -- , scoreTest ( Nano.freeTVars
  --             , (Nano.Forall "a" (Nano.Mono (TVar "a" :=> TVar "b")))
  --             , ["b"]
  --             , 1
  --             , "freeTVars 5")
  , scoreTest ( mconcat (map ((Any .) . Nano.occursIn) ["a", "b", "c", "d"])
              , TInt
              , Any False
              , 1
              , "occursIn 6")
   -- , scoreTest ( Nano.freeTVars
   --            , (Nano.Forall "a" (Nano.Mono (Nano.TVar "a" :=> Nano.TVar "a")))
   --            , []
   --            , 1
   --            , "freeTVars 7")
   -- 1b tests
   , scoreTest ( Nano.substituteAll [("a", TInt), ("g", TBool)]
               , TVar "g"
               , TBool
               , 1
               , "part 1b test 1" )
  , scoreTest ( Nano.substitute ("b", TBool)
               , TVar "a" :=> TVar "b"
               , TVar "a" :=> TBool
               , 1
               , "part 1b test 2" )
  , scoreTest ( Nano.substituteAll [("b", TInt), ("a", TList TInt)]
               , TVar "b" :=> TVar "a"
               , TInt :=> TList TInt
               , 1
               , "part 1b test 3" )
  , scoreTest ( Nano.solve
               , [ TVar "a" :~ TInt,
                   TVar "b" :~ TList (TVar "a") ]
               , Right [("a", TInt), ("b", TList TInt)]
               , 1
               , "part 1b test 4" )
   -- 2a tests
   , scoreTest ( Nano.solve
              , [TVar "a" :~ TInt]
              , Right [("a", TInt)]
              , 1
              , "unifyTVar 1")
  , scoreTest ( Nano.solve
              , [TVar "c" :~ TList (TVar "d")]
              , Right [("c", TList (TVar "d"))]
              , 1
              , "unifyTVar 2")
  , scoreTest ( Nano.solve
              , [TVar "b" :~ TVar "b"]
              , Right []
              , 1
              , "unifyTVar 3" )
  , scoreTest ( Nano.solve
              , [TVar "d" :~ TVar "a" :=> TVar "d"]
              , Left . Error $ "cannot unify `d` and `(a) -> d`"
              , 1
              , "unifyTVar 4")
  -- 2b tests
  , scoreTest ( Nano.solve
              , [TInt :~ TInt]
              , Right []
              , 1
              , "unify 1" )
  , scoreTest ( Nano.solve
              , [TInt :~ TBool]
              , Left . Error $ "cannot unify `Int` and `Bool`"
              , 1
              , "unify 2" )
  , scoreTest ( Nano.solve
              , [TInt :=> TInt :~ TVar "a" :=> TVar "a"]
              , Right [("a", TInt)]
              , 1
              , "unify 3" )
  , scoreTest ( Nano.solve
              , [TInt :~ TInt :=> TInt]
              , Left . Error $ "cannot unify `Int` and `(Int) -> Int`"
              , 1
              , "unify 4" )
  -- 3a tests
  , fileTest  ( "tests/input/3atest1.hs"
              , Right TBool
              , 1 )
  , fileTest  ( "tests/input/3atest2.hs"
              , Right TInt
              , 1 )
  , fileTest  ( "tests/input/3atest3.hs"
              , Right TInt
              , 1 )
  , fileTest ( "tests/input/3atest4.hs"
              , Right TInt
              , 1 )
  -- 3b tests
  -- , scoreTest ( uncurry Nano.generalize
  --             , ([], ((Nano.TVar "a") Nano.:=> (Nano.TVar "a")))
  --             , Nano.Forall "a" $ Nano.Mono $ (Nano.TVar "a") Nano.:=> (Nano.TVar "a")
  --             , 1
  --             , "generalize 1")
  -- , scoreTest ( uncurry Nano.generalize
  --             , ([("x", Nano.Mono $ Nano.TVar "a")], ((Nano.TVar "a") Nano.:=> (Nano.TVar "a")))
  --             , Nano.Mono $ (Nano.TVar "a") Nano.:=> (Nano.TVar "a")
  --             , 1
  --             , "generalize 2")
  -- , scoreTest ( sort . boundVars . uncurry Nano.generalize
  --             , ([], ((Nano.TVar "a") Nano.:=> ((Nano.TVar "b") Nano.:=> (Nano.TVar "c"))))
  --             , ["a","b","c"]
  --             , 1
  --             , "generalize 3")
  -- , scoreTest ( uncurry Nano.instantiate
  --             , (2, Nano.Forall "h" $ Nano.Mono $ Nano.TList (Nano.TVar "h"))
  --             , (3, Nano.TList (Nano.TVar "a2"))
  --             , 1
  --             , "instantiate 1")
  -- , scoreTest ( uncurry Nano.instantiate
  --             , (2, Nano.Forall "a" $ Nano.Forall "b" $ Nano.Mono $ (Nano.TVar "a") Nano.:=> (Nano.TVar "b"))
  --             , (4, (Nano.TVar "a2") Nano.:=> (Nano.TVar "a3"))
  --             , 1
  --             , "instantiate 2")
  -- , scoreTest ( uncurry Nano.instantiate
  --             , (2, Nano.Forall "a" $ Nano.Mono $ (Nano.TVar "a") Nano.:=> (Nano.TVar "b"))
  --             , (3, (Nano.TVar "a2") Nano.:=> (Nano.TVar "b"))
  --             , 1
  --             , "instantiate 3")
  , fileTest  ( "tests/input/3btest1.hs"
              , Right TBool
              , 2 )
  , fileTest  ( "tests/input/3btest2.hs"
              , Right TInt
              , 2 )
  -- 3c tests
  , fileTest  ( "tests/input/3ctest1.hs"
              , Right TInt
              , 1 )
  , fileTest  ( "tests/input/3ctest2.hs"
              , Right TBool
              , 1 )
  , fileTestE  ( "tests/input/3ctest3.hs"
              , "cannot unify"
              , 1 )
  , fileTestE  ( "tests/input/3ctest4.hs"
              , "cannot unify"
              , 1 )
  , fileTest  ( "tests/input/3ctest5.hs"
              , Right TBool
              , 1 )
  , fileTestE  ( "tests/input/3ctest6.hs"
              , "cannot unify"
              , 1 )
  , fileTest  ( "tests/input/3ctest7.hs"
              , Right $ TInt :=> TInt
              , 2 )
  , fileTestE  ( "tests/input/3ctest8.hs"
              , "cannot unify"
              , 1 )
  , fileTest  ( "tests/input/3ctest9.hs"
              , Right $ TInt :=> TInt
              , 2 )
  , fileTest  ( "tests/input/3ctest10.hs"
              , Right TInt
              , 2 )
  , fileTest  ( "tests/input/3ctest11.hs"
              , Right TInt
              , 1 )
  , fileTestE  ( "tests/input/3ctest12.hs"
              , "cannot unify"
              , 1 )
  -- 3d tests
  , fileTest  ( "tests/input/3dtest1.hs"
              , Right $ TList TInt
              , 2 )
  , fileTest  ( "tests/input/3dtest2.hs"
              , Right TInt
              , 2 )
  , fileTestE  ( "tests/input/3dtest3.hs"
              , "cannot unify"
              , 2 )
  , fileTestE  ( "tests/input/3dtest4.hs"
              , "cannot unify"
              , 3 )
  , fileTestE  ( "tests/input/3dtest5.hs"
              , "cannot unify"
              , 3 )
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

    fileTest  (f, r, n) = scoreTest' sc (Nano.typeOfFile, f, r, n, "file: " ++ f)
    fileTestE (f, e, n) = scoreTest' sc (fmap (expectError e) . Nano.typeOfFile, f, True, n, "file: " ++ f)

    expectError :: String -> Either Error a -> Bool
    expectError err = either (isInfixOf err . Nano.errMsg) (const False)
