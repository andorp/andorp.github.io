# Unification with Backtracking monad

## The story

After watching [John Hughes' lecture on Monad](http://www.cse.chalmers.se/~rjmh/OPLSS/), I decided to reimplement the logic he introduced from
zero useing the newest version of the [Logic](https://hackage.haskell.org/package/logict) haskell package. Fortunately the interface was easy to reuse.

In the future I would like to reuse the unification logic to implement some tiny compilers in Haskell.

## The code

### Imports

No much import is necessary and generalized newtype deriving saves us lot of time.

    {-# LANGUAGE GeneralizedNewtypeDeriving #-}
    module Main where

    import Control.Monad
    import Control.Applicative
    import qualified Control.Monad.Logic as L
    import Control.Monad.IO.Class
    import Data.IORef

### Monadic interface

Logic search is monadic and it is built on top of IO. In this was we can create logical variables
on the easy way. `MonadPlus` and `MonadLogic` instances are stands for the reuseability of the
`LogicT` backtracking monad.

    newtype Logic a = Logic { unLogic :: L.LogicT IO a }
      deriving (Functor, Applicative, Alternative, Monad, L.MonadPlus, L.MonadLogic, MonadIO)

As we are interested in any solutions, we need to unwrap the Logic constructor and use `observeManyT`
combinator of the `LogicT` library.

    run :: Logic a -> IO [a]
    run = L.observeManyT . unLogic

### Logical variables

A logical variable is a reference cell in the memory that could be empty,
or can hold a value or a link to another logical variable, which link is
created during the unification process.

    type LogicVar a = IORef (Maybe (Logical a))

    data Logical a = Value a | Var (LogicVar a)

Creating a new variable, creates a new `IORef` wrapped into a `Var`.

    variable :: Logic (Logical a)
    variable = liftM Var (liftIO (newIORef Nothing))

We need to check if a chain of variables at the end contains a value or ends
up in a variable witout any value.

    follow :: Logical a -> Logic (Logical a)
    follow (Value a) = return (Value a)
    follow (Var r) = do
      v <- liftIO $ readIORef r
      case v of
        Nothing -> return (Var r)
        Just  v -> follow v

### Unification

Unification works on two things with the same type. Unification differs on different types,
so forth, we need to define a type class for it. Unification is a Logic computation which
could succeed without any relevant information, or could fail.

    class Unifiable a where
      unify :: a -> a -> Logic ()

Unification of two integer values is to check if those values are the same. The `guard` combinator
is perfect to do the job for us.

    instance Unifiable Integer where
      unify a b = guard (a == b)

Unification of two logical values is to follow the two values. There are four different
cases.

 * If the variables are at the end of the chains, we have to check it they are the same.
In that case, we are ok.
 * If the first chain ends up in a variable, we link that to the second end of the chain,
no matter if it is a variable or a value.
 * If the second chain ends up in a variable, we link that to the end of the first chain, which
must be a value.
 * If both ends of the chains are values, we need to unify them.

Let's see.

    instance Unifiable a => Unifiable (Logical a) where
      unify a b = do
        a' <- follow a
        b' <- follow b
        case (a',b') of
          (Var ra, Var rb) | ra == rb -> return ()
          (Var ra, _) -> instantiate ra b'
          (_, Var rb) -> instantiate rb a'
          (Value a, Value b) -> unify a b

Instantiation is the process how we link to variables together. The clause after `mplus` supports
the clean-up of the given value in any case of failure, and after cleaning up, the computation fails,
and continues on backtracking.

    instantiate r v =
      liftIO (writeIORef r (Just v)) `mplus` do { liftIO (writeIORef r Nothing); mzero }

To avoid lot of boilerplate code, we define a class for Haskell types that can be represented
in our logic computations. The `LogicRepr` typeclass is for that, and gives us default implementations.

    class LogicRepr a where
      toLogical :: a -> Logical a
      toLogical = Value
      fromLogical :: Logical a -> Logic a
      fromLogical = value

The default implementaion claims a logical variable to have a value, or the computation for
the conversion should fail.

    value :: Logical a -> Logic a
    value l = do
      l <- follow l
      case l of
        Value a -> return a
        Var _ -> mzero

In our example we use only integers...

    instance LogicRepr Integer where

... and logical lists, which are differents from haskell list in a sense, that
they can contain places for further unifications.

    data List a = Nil | Cons a (Logical (List a))

Unification of a logical list is straightforward.

    instance Unifiable a => Unifiable (List a) where
      unify Nil Nil = return ()
      unify (Cons x xs) (Cons y ys) = do
        unify x y
        unify xs ys
      unify _ _ = mzero

We can convert easily from `Haskell List`s into `Logical List`s

    toLogicalList :: (LogicRepr a) => [a] -> Logical (List (Logical a))
    toLogicalList []     = Value Nil
    toLogicalList (x:xs) = Value $ Cons (toLogical x) (toLogicalList xs)

Also we can convert easily from `Logical List`s to `Haskell List`s

    fromLogicalList :: (LogicRepr a) => Logical (List (Logical a)) -> Logic [a]
    fromLogicalList l = do
      l <- value l
      case l of
        Nil -> return []
        Cons x xs -> do
          x <- value x
          xs <- fromLogicalList xs
          return (x:xs)

### Tests

Let's run some tests...

    test1 :: Integer -> Logic ()
    test1 n = do
      x <- variable
      y <- variable
      unify x y
      unify y (toLogical n)
      x' <- fromLogical x
      liftIO $ print x'

We can check if prolog append example works as expected.

    -- Prolog append example
    -- append([], Ys, Ys).
    -- append(X|Xs, Ys, Zs) :- append(Xs, Ys, X|Zs)

    appendL xs ys zs =
      (do unify xs (Value Nil)
          unify ys zs)
      `mplus`
      (do x   <- variable
          xs' <- variable
          zs' <- variable
          unify xs (Value (Cons x xs'))
          unify zs (Value (Cons x zs'))
          appendL xs' ys zs')

    test2 :: [Integer] -> Logic ([Integer], [Integer])
    test2 zs = do
      xs <- variable
      ys <- variable
      appendL xs ys (toLogicalList zs)
      liftM2 (,) (fromLogicalList xs) (fromLogicalList ys)

### Main

    main = do
      run (mzero :: Logic ()) >>= print
      run (test1 10) >>= print
      run (test2 [1,2,3]) >>= print

It will print

    []
    10
    [()]
    [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]

## Conclusion

It is easy to write a simple unification algorithm is one can use the existing backtracking monad
in Haskell. The code can be found [here.](https://andorp.github.io/asset/haskell/logic.hs)