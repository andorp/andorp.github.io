{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad
import Control.Applicative
import qualified Control.Monad.Logic as L
import Control.Monad.IO.Class
import Data.IORef

newtype Logic a = Logic { unLogic :: L.LogicT IO a }
  deriving (Functor, Applicative, Alternative, Monad, L.MonadPlus, L.MonadLogic, MonadIO)

run :: Logic a -> IO [a]
run = L.observeAllT . unLogic

data Logical a = Value a | Var (LogicVar a)

type LogicVar a = IORef (Maybe (Logical a))

variable :: Logic (Logical a)
variable = liftM Var (liftIO (newIORef Nothing))

follow :: Logical a -> Logic (Logical a)
follow (Value a) = return (Value a)
follow (Var r) = do
  v <- liftIO $ readIORef r
  case v of
    Nothing -> return (Var r)
    Just  v -> follow v

class Unifiable a where
  unify :: a -> a -> Logic ()

instance Unifiable Integer where
  unify a b = guard (a == b)

instance Unifiable a => Unifiable (Logical a) where
  unify a b = do
    a' <- follow a
    b' <- follow b
    case (a',b') of
      (Var ra, Var rb) | ra == rb -> return ()
      (Var ra, _) -> instantiate ra b'
      (_, Var rb) -> instantiate rb a'
      (Value a, Value b) -> unify a b

instantiate r v =
  liftIO (writeIORef r (Just v)) `mplus` do { liftIO (writeIORef r Nothing); mzero }

value :: Logical a -> Logic a
value l = do
  x <- follow l
  case x of
    Value y -> return y
    Var _   -> mzero

class LogicRepr a where
  toLogical :: a -> Logical a
  toLogical = Value
  fromLogical :: Logical a -> Logic a
  fromLogical = value

instance LogicRepr Integer

data List a = Nil | Cons a (Logical (List a))

instance Unifiable a => Unifiable (List a) where
  unify Nil Nil = return ()
  unify (Cons x xs) (Cons y ys) = do
    unify x y
    unify xs ys
  unify _ _ = mzero

toLogicalList :: (LogicRepr a) => [a] -> Logical (List (Logical a))
toLogicalList []     = Value Nil
toLogicalList (x:xs) = Value $ Cons (toLogical x) (toLogicalList xs)

fromLogicalList :: (LogicRepr a) => Logical (List (Logical a)) -> Logic [a]
fromLogicalList l = do
  l <- value l
  case l of
    Nil -> return []
    Cons x xs -> do
      x <- value x
      xs <- fromLogicalList xs
      return (x:xs)

test1 :: Integer -> Logic ()
test1 n = do
  x <- variable
  y <- variable
  unify x y
  unify y (toLogical n)
  x' <- fromLogical x
  liftIO $ print x'

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

test3 = do
  x <- variable
  unify x (Value Nil :: Logical (List (Logical Integer)))
  fromLogicalList x

test4 = do
  x <- variable
  unify x (Value (Cons (Value (3 :: Integer)) (Value Nil)))
  fromLogicalList x

test5 = do
  x <- variable
  unify x (Value (Cons (Value (3 :: Integer)) (Value (Cons (Value 4) (Value Nil)))))
  fromLogicalList x

main = do
  run (mzero :: Logic ()) >>= print
  run (test1 10) >>= print
  run test3 >>= print
  run test4 >>= print
  run test5 >>= print
  run (test2 [1,2,3]) >>= print

