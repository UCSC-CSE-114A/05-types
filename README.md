# Assignment 5: Nano Types (240 points)

## Due by Wednesday 6/5 by 11:59pm


## Overview

The overall objective of this assignment is to
better understand the notions of type checking, type inference, and polymorphism
by implementing a type inference algorithm for Nano.

The assignment is in the file:

+ [TypeCheck.hs](/src/Language/Nano/TypeCheck.hs)

and

+ [tests/Test.hs](/tests/Test.hs) has some sample tests,
  and testing code that you will use to check your
  assignments before submitting.

You should only need to modify the parts of the files which say:

```haskell
error "TBD: ..."
```

with suitable Haskell implementations.

**Note:** Start early! Type inference has many subtle points that can take a while to get right.


## Assignment Testing and Evaluation


Most of the points, will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ stack test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error ...` with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

## Submission Instructions

Please commit all your code to gitlab, and indicate in your canvas submission your commit ID and your partner. We will be grading the code from gitlab, so ensure what you add to gitlab is what you want graded. 

## Data Structures and Overview

This assignment builds upon our previous implementation of Nano from HW4,
so most data types in `Types.hs` are the same.
We have added the following definitions to support type inference.

### Types

We will use the following Haskell datatypes to represent Nano's types and poly-types:

```haskell
data Type
  = TInt             -- Int
  | TBool            -- Bool
  | Type :=> Type    -- function type: T1 -> T2
  | TVar TVar        -- type variable: a, b, c
  | TList Type       -- list type: [T]
    
data Poly = Mono Type        -- mono-type 
          | Forall TVar Poly -- polymorphic type
```

where `TVar` is just a type alias for `String` used to represent type variable names:

```haskell
type Id = String
```

For example, a Nano type `Int -> Int` is represented inside your type checker as:

```haskell
TInt :=> TInt
```

whereas a polymorphic Nano type `forall a . List a -> a` is represented as:

```haskell
Forall "a" (Mono (TList (TVar "a") :=> TVar "a"))
```

(or simply `forall "a" (list "a" :=> "a")` using convenience functions `forall` and `list`, also defined in `Types.hs`).

### Type Environment

A **type environment** is simply a dictionary that maps program variables to poly-types.
As before, the dictionary is implemented as a list of pairs:

```haskell
type TypeEnv = [(Id, Poly)]
```

### Type Substitution

A **type substitution** is also a dictionary, but it maps *type variables* to *types* they should be replaced with:

```haskell
type Subst = [(TVar, Type)] 
```

Throughout the assignment, make sure that the keys (type variables) in the substitution are *unique*.
You also get to *assume* this property when you implement operations on substitutions.



## Problem 1: Warm-up (70 points)

In this problem, you will implement some helper functions for your type-checker
which you will need later on.


### (a) Free type variables: 20 points

First implement two functions that compute the list of *free type variables*
in a type and in a poly-type.
Note: to enable overloading (using a function with the same name for types and poly-types),
this function is defined as part of a type class `HasTVars`.

```haskell
instance HasTVars Type where
  freeTVars t     = error "TBD: ..."

instance HasTVars Poly where
  freeTVars s     = error "TBD: ..."
```

As always, you are allowed (and encouraged) to replace `freeTVars t` and `freeTVars s` with multiple equations for different patterns.

Throughout this assignment, you are allowed to use any library functions,
as long as you don't add new `import` statements.
In particular, some useful functions from the `List` library include `delete`, `nub`, and `\\` (look them up on Hoogle).
Note: the `List` library is imported *qualified*:

```haskell
import qualified Data.List as L
```

So whenever you want to use a function from that library, you have to prefix its name with `L.` (e.g. `L.delete`).

When you are done you should get the following behavior:

```haskell
>>> freeTVars TInt
[]

>>> freeTVars (TVar "a")
["a"]

>>> freeTVars (forall "a" (list "a" :=> "a"))
[]
```


### (b) Substitutions: 50 points

Implement a function to lookup what a type variable maps to in a substitution: 

```haskell
lookupTVar :: TVar -> Subst -> Type
```
and a function to remove the mapping for a given type variable from a substitution
(recall that all variables in a substitution are supposed to be unique):

```haskell
removeTVar :: TVar -> Subst -> Subst
```
When you are done you should get the following behavior:

```haskell
>>> lookupTVar "a" [("a", TInt)]
Int

>>> lookupTVar "a" [("b", TInt)]
a

>>> removeTVar "a" [("a", TInt)]
[]

>>> removeTVar "a" [("b", TInt)]
[("b",Int)]
```

Next, use `lookupTVar` and `removeTVar` to write 
two functions that *apply a substitution*
to a type and to a poly-type
(both named `apply`):

```haskell
instance Substitutable Type where  
  apply sub t         = error "TBD: ..."

instance Substitutable Poly where    
  apply sub p         = error "TBD: ..."
```

Once you have implemented this functionality and
recompiled, you should get the following behavior:

```haskell
>>> apply [("a",TInt)] (list "a")
[Int]

>>> apply [("a",TInt)] (forall "a" $ list "a")
forall a . [a]
```

Finally, use `apply` to implement the function `extendSubst`,
which extends a substitution with a new type assignment:

```haskell
extendSubst :: Subst -> TVar -> Type -> Subst
```
When you are done you should get the following behavior:

```haskell
>>> extendSubst [("a", TInt)] "b" TBool
[("b",Bool), ("a",Int)]

>>> extendSubst [("a", list "b")] "b" TBool
[("b",Bool), ("a",[Bool])]
```


## Problem 2: Unification (50 points)

Unification is a crucial part of type inference.
In a nutshell, the inference algorithm works by:
  
  * recursively traversing the program, assembling expression types from the types of their sub-expressions;
  * when some type is not yet known, it picks a **fresh type variable** to represent this type;
  * whenever two types must be the same, the algorithm tries to **unify** them and figure out what these type variables actually stand for.

A *fresh type variable* is a type variable that is distinct from all type variables the algorithm has generated before.
Our implementation will name the fresh type variables `a0, a1, ...`.
In order to remember the index of the first unused fresh varaible,
we introduce a new datatype:

```haskell
data InferState = InferState { 
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
}
```

which represents the "current state" of the unification and inference algorithms.
It remembers how many fresh type variables the algorithm has generated so far,
alongside the "current substitution".

### (a) Unification for type variables: 20 points

Implement a function `unifyTVar st a t`
that tries to unify a type variable `a` with a type `t`.
If it succeeds, it records the new type assignment by extending the state `st`.
If it fails, it throws an exception.

```haskell
unifyTVar :: InferState -> TVar -> Type -> InferState
```

You can use a helper function `extendState` to extend a substitution inside a state.

**Assumption:** 
You can *assume* that `a` does not appear in the *domain* of `stSub st`
(i.e. among the type variables it maps);
remember to *guarantee* this property whenever you call `unifyTVar`.

When you are done you should get the following behavior:

```haskell
>>> stSub $ unifyTVar initInferState "a" (list "b")
[("a",[b])]

>>> stSub $ unifyTVar initInferState "a" "a"
[]

>>> stSub $ unifyTVar initInferState "a" (list "a")
*** Exception: Error {errMsg = "type error: cannot unify a and [a] (occurs check)"}
```

### (b) Unification for types: 30 points

Implement a function `unify st t1 t2`
that tries to unify the types `t1` and `t2`.
If it succeeds, it records the new type assignment by extending the state `st`.
If it fails, it throws an exception.

```haskell
unify :: InferState -> Type -> Type -> InferState
```

**Assumption:** 
You can *assume* that any type variables in `t1` and `t2` do not appear in the domain of `stSub st`;
remember to *guarantee* this property whenever you call `unify`.

When you are done you should get the following behavior:

```haskell
>>> stSub $ unify initInferState TInt TInt
[]

>>> stSub $ unify initInferState TInt TBool
*** Exception: Error {errMsg = "type error: cannot unify Int and Bool"}

>>> stSub $ unify initInferState ("a" :=> TInt) (TBool :=> "b")
[("b",Int),("a",Bool)]

>>> stSub $ unify initInferState (list "a") (list $ list "a")
*** Exception: Error {errMsg = "type error: cannot unify a and [a] (occurs check)"}
```

## Problem 3: Type Inference (120 points)

Now we have everything in place to implement type inference!

### (a) Mono-types: 20 points

Let's first consider the fragment of the language without:

  * binary operators
  * conditionals
  * let polymorphism
  * recursion
  * lists

Implement the function `infer st gamma e` for this fragment.
This function attempts to infer the type of `e` in type environment `gamma`
given that the current state is `st`.
If `e` imposes constraints on the types of its sub-expressions, 
`infer` calls `unify` to enforce these constraints. 
If all unification steps succeed, `infer` returns the inferred type of `e` and a new state 
(possibly extended with new type assignments generated during unification).

In this part, you can assume that the type environment maps all variables to mono-types, i.e. `Mono t`.

When you are done, and replace `Lexer.x` and `Parser.y` with a working lexer and parser from HW4,
your code should pass the first 5 test cases.
In particular, you should observe the following behavior:

```haskell
>>> typeOfString "True"
Bool

>>> typeOfString "1"
Int

>>> typeOfString "(\\x -> x) 5"
Int

>>> typeOfString "(\\x -> x + 1) True"
*** Exception: Error {errMsg = "type error: cannot unify Int and Bool"}
```

**Note:** if you don't have a working Nano parser yet, use the function `typeOfExpr` instead and write ASTs by hand.


### (b) Polymorphism: 50 points

Now let's add support for polymorphism!

  * In this version, variables in the type environment can have polymorphic types, like `forall a . a -> a`.
  
  * Whenever we use a variable, we have to **instantiate** its poly-type, 
    i.e. replace all bound type variable with *fresh free type variables*.
    
  * Whenever we define a variable using a `let`, 
    we have to **generalize** its type into a poly-type.

First implement the function `generalize gamma t` that generalizes `t` into a poly-type,
i.e. binds all its type variables that are *not free* in `gamma` with a `Forall`:

```haskell
generalize :: TypeEnv -> Type -> Poly
```

When you are done you should observe the following behavior:

```haskell
>>> generalize [] ("a" :=> "a")
forall a . (a) -> a

>>> generalize [("x", Mono "a")] ("a" :=> "a")
(a) -> a
```

Next implement the function `instantiate n s` that instantiates a poly-type `s`
by replacing its bound type variables with fresh type variables.
Here `n` is the index of the first unused fresh type variable.

```haskell
instantiate :: Int -> Poly -> (Int, Type)
```

**Assumption:** 
In `instantiate n s`, you can assume that bound type variables in `s`
cannot clash with `ai` where `i >= n`. 
Remember to *guarantee* this property whenever you construct your own polymorphic types.

When you are done you should observe the following behavior:

```haskell
>>> instantiate 0 (Forall "a" (Forall "b" ("a" :=> "b")))
(2,(a0) -> a1)
```

Finally, modify the function `infer` to support polymorphism.

When you are done, your implementation should be able to 
type-check the "double identity" example from the lecture:

```haskell
>>> typeOfString "let id = \\x -> x in let y = id 5 in id (\\z -> z + y)"
(Int) -> Int
```

### (c) Built-in functions: 20 points

Instead of implementing separate type inference for binary operators, conditionals, and list,
we decided to represent them as **built-in functions**
and reuse the type inference for variables and function application.

Fill in the types of built-in functions inside `preludeTypes`
(we have pre-filled the one for `+`).
Remember that you are not allowed to use `a0, a1, ...` as bound type variables,
since those are reserved for use as free type variables by the algorithm.

At this point the first 18 tests should pass.

### (d) Recursion: 30 points

Modify your implementation of `infer` to support recursive function definitions.
Once you are done, all tests should pass.


