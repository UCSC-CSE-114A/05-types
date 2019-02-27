{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Data.List (isInfixOf)
import qualified Language.Nano.Types     as Nano
import           Language.Nano.Types ( Type(..) )
import qualified Language.Nano.Eval      as Nano
import qualified Language.Nano.TypeCheck as Nano

main :: IO ()
main = runTests [ unit ]

parse = Nano.parse

unit :: Score -> TestTree
unit sc = testGroup "NANO"
  [ fileTest  ( "tests/input/t1.hs"
              , Nano.TBool
              , 1 )
  , fileTest  ( "tests/input/t2.hs"
              , Nano.TInt
              , 1 )
  , fileTest  ( "tests/input/t3.hs"
              , Nano.TInt
              , 1 )
  , fileTest ( "tests/input/t4.hs"
              , Nano.TInt
              , 1 )
  , fileTest  ( "tests/input/t5.hs"
              , Nano.TInt
              , 1 )
  , fileTest  ( "tests/input/t6.hs"
              , Nano.TBool
              , 1 )
  , fileTestE  ( "tests/input/t8.hs"
              , "type error"
              , 1 )
  , fileTestE  ( "tests/input/t9.hs"
              , "type error"
              , 1 )
  , fileTest  ( "tests/input/t10.hs"
              , Nano.TBool
              , 1 )
  , fileTestE  ( "tests/input/t11.hs"
              , "type error"
              , 1 )
  , fileTest  ( "tests/input/t12.hs"
              , Nano.TInt :=> Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/t13.hs"
              , Nano.TBool
              , 2 )
  , fileTestE  ( "tests/input/t14.hs"
              , "type error"
              , 1 )
  , fileTest  ( "tests/input/t15.hs"
              , Nano.TInt :=> Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/t16.hs"
              , Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/t17.hs"
              , Nano.TInt
              , 1 )
  , fileTestE  ( "tests/input/t18.hs"
              , "type error"
              , 1 )              
  , fileTest  ( "tests/input/t19.hs"
              , Nano.TList Nano.TInt
              , 2 )
  , fileTest  ( "tests/input/t20.hs"
              , Nano.TInt
              , 2 )
  , fileTestE  ( "tests/input/t21.hs"
              , "type error"
              , 2 )                            
  , fileTestE  ( "tests/input/t22.hs"
              , "type error"
              , 3 )                            
  , fileTestE  ( "tests/input/t23.hs"
              , "type error"
              , 3 )                            
  ]
  where
    -- scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    -- scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

    -- failTest :: (Show b, Eq b) => (a -> b, a, String, Int, String) -> TestTree
    -- failTest (f, x, err, n, msg) = scoreTest' sc (expectError err (return . f), x, True, n, msg)

    fileTest (f, r, n)  = scoreTest' sc (Nano.typeOfFile, f, r, n, "file: " ++ f)
    fileTestE (f, e, n) = scoreTest' sc (expectError e Nano.typeOfFile, f, True, n, "file: " ++ f)


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
