{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Data.List (isInfixOf, sort)
import qualified Language.Nano.Types     as Nano
import           Language.Nano.Types ( Type(..) )
import qualified Language.Nano.Eval      as Nano
import qualified Language.Nano.TypeCheck as Nano

main :: IO ()
main = runTests [ unit ]

parse = Nano.parse

unit :: Score -> TestTree
unit sc = testGroup "NANO"
  [ -- 1a tests
    scoreTest ( Nano.freeTVars
              , (TVar "a")
              , ["a"]
              , 1 -- points for this test case
              , "freeTVars 1")
  , scoreTest ( Nano.freeTVars
              , (TList (TList (TVar "a")))
              , ["a"]
              , 1
              , "freeTVars 2")
  , scoreTest ( Nano.freeTVars
              , (TVar "a" :=> TVar "b")
              , ["a", "b"]
              , 1
              , "freeTVars 3")
  , scoreTest ( Nano.freeTVars
              , (TVar "a" :=> TVar "a")
              , ["a"]
              , 1
              , "freeTVars 4")
  , scoreTest ( Nano.freeTVars
              , (Nano.Forall "a" (Nano.Mono (Nano.TVar "a" :=> Nano.TVar "b")))
              , ["b"]
              , 1
              , "freeTVars 5")
  , scoreTest ( Nano.freeTVars
              , (TInt)
              , []
              , 1
              , "freeTVars 6")
  , scoreTest ( Nano.freeTVars
              , (Nano.Forall "a" (Nano.Mono (Nano.TVar "a" :=> Nano.TVar "a")))
              , []
              , 1
              , "freeTVars 7")
  , scoreTest ( Nano.freeTVars
              , (Nano.Forall "a" (Nano.Mono (Nano.TBool :=> Nano.TBool)))
              , []
              , 1
              , "freeTVars 8")
  -- 1b tests
  , scoreTest ( uncurry Nano.lookupTVar
               , ("a", [("a", TInt)])
               , TInt
               , 1
               , "lookupTVar 1" )
  , scoreTest ( uncurry Nano.lookupTVar
               , ("g", [("g", TBool), ("a", TInt)])
               , TBool
               , 1
               , "lookupTVar 2" )
  , scoreTest ( uncurry Nano.lookupTVar
               , ("a", [("g", TBool)])
               , (TVar "a")
               , 1
               , "lookupTVar 3" )
  , scoreTest ( uncurry Nano.removeTVar
               , ("a",[("a", TInt)])
               , []
               , 1
               , "removeTVar 1" )
  , scoreTest ( uncurry Nano.removeTVar
               , ("a",[("a", TInt), ("b", TBool)])
               , [("b", TBool)]
               , 1
               , "removeTVar 2" )
  , scoreTest ( uncurry Nano.removeTVar
               , ("a",[("b", TBool)])
               , [("b", TBool)]
               , 1
               , "removeTVar 3" )
  , scoreTest ( Nano.apply [("a", TInt)]
               , TList (TVar "a")
               , TList (TInt)
               , 1
               , "apply 1" )
  , scoreTest ( Nano.apply [("a", TInt)]
               , TList (TVar "b")
               , TList (TVar "b")
               , 1
               , "apply 2" )
  , scoreTest ( Nano.apply [("a", TList (TInt)), ("b", TInt)]
               , TVar "b" :=> TVar "a"
               , TInt :=> TList (TInt)
               , 1
               , "apply 3" )
  , scoreTest ( Nano.apply [("a", TInt)]
               , Nano.forall "a" (Nano.list "a")
               , Nano.forall "a" (Nano.list "a")
               , 1
               , "apply 4" )
  , scoreTest ( Nano.apply [("b", TInt)]
               , Nano.forall "a" ("a" :=> "b")
               , Nano.forall "a" ("a" :=> TInt)
               , 1
               , "apply 5" )
  , scoreTest ( uncurry (Nano.extendSubst [])
               , ("b", TBool)
               , [("b", TBool)]
               , 1
               , "extendSubst 1" )
  , scoreTest ( uncurry (Nano.extendSubst [("a", TInt)])
               , ("b", TBool)
               , [("b", TBool), ("a", TInt)]
               , 1
               , "extendSubst 2" )
  , scoreTest ( uncurry (Nano.extendSubst [("a", TInt)])
               , ("b", (TList (TVar "a")))
               , [("b", TList (TInt)), ("a", TInt)]
               , 1
               , "extendSubst 3" )
  , scoreTest ( uncurry (Nano.extendSubst [("a", TList (TVar "b"))])
               , ("b", TBool)
               , [("b", TBool), ("a", TList TBool)]
               , 1
               , "extendSubst 4" )
  , scoreTest ( uncurry (Nano.extendSubst [("a", TList (TVar "b")), ("c", TVar "b")])
               , ("b", TBool)
               , [("b", TBool), ("a", TList TBool), ("c", TBool)]
               , 1
               , "extendSubst 5" )
  -- 2a tests
  , scoreTest ( Nano.stSub . uncurry (Nano.unifyTVar Nano.initInferState)
              , ("a", Nano.TInt)
              , [("a", Nano.TInt)]
              , 1
              , "unifyTVar 1")
  , scoreTest ( Nano.stSub . uncurry (Nano.unifyTVar Nano.initInferState)
              , ("c", Nano.TList (Nano.TVar "d"))
              , [("c", Nano.TList (Nano.TVar "d"))]
              , 1
              , "unifyTVar 2")
  , scoreTest ( Nano.stSub . uncurry (Nano.unifyTVar Nano.initInferState)
              , ("b", Nano.TVar "b")
              , []
              , 1
              , "unifyTVar 3" )
  , failTest  ( Nano.stSub . uncurry (Nano.unifyTVar Nano.initInferState)
              , ("d", (Nano.TVar "a") Nano.:=> (Nano.TVar "d"))
              , "type error"
              , 1
              , "unifyTVar 4")
  -- 2b tests
  , scoreTest ( Nano.stSub . uncurry (Nano.unify Nano.initInferState)
              , (Nano.TInt, Nano.TInt)
              , []
              , 1
              , "unify 1" )
  , failTest ( Nano.stSub . uncurry (Nano.unify Nano.initInferState)
              , (Nano.TInt, Nano.TBool)
              , "type error"
              , 1
              , "unify 2" )
  , scoreTest ( Nano.stSub . uncurry (Nano.unify Nano.initInferState)
              , (Nano.TInt Nano.:=> Nano.TInt, "a" Nano.:=> "a")
              , [("a", Nano.TInt)]
              , 1
              , "unify 3" )
  , failTest ( Nano.stSub . uncurry (Nano.unify Nano.initInferState)
              , (Nano.TInt, Nano.TInt Nano.:=> Nano.TInt)
              , "type error"
              , 1
              , "unify 4" )
  -- 3a tests
  , fileTest  ( "tests/input/3atest1.hs"
              , Nano.TBool
              , 1 )
  , fileTest  ( "tests/input/3atest2.hs"
              , Nano.TInt
              , 1 )
  , fileTest  ( "tests/input/3atest3.hs"
              , Nano.TInt
              , 1 )
  , fileTest ( "tests/input/3atest4.hs"
              , Nano.TInt
              , 1 )
  -- 3b tests
  , scoreTest ( uncurry Nano.generalize
              , ([], ((Nano.TVar "a") Nano.:=> (Nano.TVar "a")))
              , Nano.Forall "a" $ Nano.Mono $ (Nano.TVar "a") Nano.:=> (Nano.TVar "a")
              , 1
              , "generalize 1")
  , scoreTest ( uncurry Nano.generalize
              , ([("x", Nano.Mono $ Nano.TVar "a")], ((Nano.TVar "a") Nano.:=> (Nano.TVar "a")))
              , Nano.Mono $ (Nano.TVar "a") Nano.:=> (Nano.TVar "a")
              , 1
              , "generalize 2")
  , scoreTest ( sort . boundVars . uncurry Nano.generalize
              , ([], ((Nano.TVar "a") Nano.:=> ((Nano.TVar "b") Nano.:=> (Nano.TVar "c"))))
              , ["a","b","c"]
              , 1
              , "generalize 3")
  , fileTest  ( "tests/input/3btest1.hs"
              , Nano.TBool
              , 2 )
  , fileTest  ( "tests/input/3btest2.hs"
              , Nano.TInt
              , 2 )
  -- 3c tests
  , fileTest  ( "tests/input/3ctest1.hs"
              , Nano.TInt
              , 1 )
  , fileTest  ( "tests/input/3ctest2.hs"
              , Nano.TBool
              , 1 )
  , fileTestE  ( "tests/input/3ctest3.hs"
              , "type error"
              , 1 )
  , fileTestE  ( "tests/input/3ctest4.hs"
              , "type error"
              , 1 )
  , fileTest  ( "tests/input/3ctest5.hs"
              , Nano.TBool
              , 1 )
  , fileTestE  ( "tests/input/3ctest6.hs"
              , "type error"
              , 1 )
  , fileTest  ( "tests/input/3ctest7.hs"
              , Nano.TInt :=> Nano.TInt
              , 2 )
  , fileTestE  ( "tests/input/3ctest8.hs"
              , "type error"
              , 1 )
  , fileTest  ( "tests/input/3ctest9.hs"
              , Nano.TInt :=> Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/3ctest10.hs"
              , Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/3ctest11.hs"
              , Nano.TInt
              , 1 )
  , fileTestE  ( "tests/input/3ctest12.hs"
              , "type error"
              , 1 )
  -- 3d tests
  , fileTest  ( "tests/input/3dtest1.hs"
              , Nano.TList Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/3dtest2.hs"
              , Nano.TInt
              , 2 )
  , fileTestE  ( "tests/input/3dtest3.hs"
              , "type error"
              , 2 )
  , fileTestE  ( "tests/input/3dtest4.hs"
              , "type error"
              , 3 )
  , fileTestE  ( "tests/input/3dtest5.hs"
              , "type error"
              , 3 )
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

    failTest :: (Show b, Eq b) => (a -> b, a, String, Int, String) -> TestTree
    failTest (f, x, err, n, msg) = scoreTest' sc (expectError err (return . f), x, True, n, msg)

    fileTest (f, r, n)  = scoreTest' sc (Nano.typeOfFile, f, r, n, "file: " ++ f)
    fileTestE (f, e, n) = scoreTest' sc (expectError e Nano.typeOfFile, f, True, n, "file: " ++ f)

    boundVars :: Nano.Poly -> [String]
    boundVars (Nano.Mono _) = []
    boundVars (Nano.Forall v t) = v:boundVars t


expectError :: (Show b) => String -> (a -> IO b) -> a -> IO Bool
expectError err f x = do { r <- f x; print r; return False }
                      `catch`
                      (return . isInfixOf err . Nano.errMsg)

env1 :: Nano.Env
env1 =
  [ ("c0", Nano.VInt 0)
  , ("c1", Nano.VInt 1)
  , ("c2", Nano.VInt 2)
  , ("c3", Nano.VInt 3)
  , ("c0", Nano.VInt 4)
  , ("c1", Nano.VInt 5)
  ]

env2 :: Nano.Env
env2 = env1 ++
  [ ("bt", Nano.VBool True)
  , ("bf", Nano.VBool False)
  ]
