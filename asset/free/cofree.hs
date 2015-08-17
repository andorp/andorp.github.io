{-# LANGUAGE FlexibleContexts #-} -- For instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a) where
{-# LANGUAGE UndecidableInstances #-} -- For instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a) where
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Applicative

class TwoButton a where
  press :: a -> (a, a)

pressLeft, pressRight :: (TwoButton a) => a -> a
pressLeft  = fst . press
pressRight = snd . press

class Comonad w where
  extract   :: w a -> a
  duplicate :: w a -> w (w a)

data CofreeTwoButton_ = Memo_ CofreeTwoButton_ CofreeTwoButton_

memoiseTwoButton_ :: (TwoButton m) => m -> CofreeTwoButton_
memoiseTwoButton_ m = Memo_ (memoiseTwoButton_ (pressLeft m)) (memoiseTwoButton_ (pressRight m))

data CofreeTwoButton a = Memo a (CofreeTwoButton a) (CofreeTwoButton a)

memoiseTwoButton :: (TwoButton m) => (m -> a) -> m -> CofreeTwoButton a
memoiseTwoButton f m = Memo (f m) (memoiseTwoButton f (pressLeft m)) (memoiseTwoButton f (pressRight m))

-- "A cofree Comonad is basically a memoised comonad. So the datastructure is"

data Cofree f a = Cofree a (f (Cofree f a))

instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a) where
  show (Cofree a fa) = concat [show a, " ", show fa]

instance Functor f => Functor (Cofree f) where
  fmap f (Cofree a fs) = Cofree (f a) (fmap (fmap f) fs)

instance Functor f => Comonad (Cofree f) where
  extract   (Cofree a _)    = a
  duplicate c@(Cofree _ fs) = Cofree c (fmap duplicate fs)

memoiseComonad :: (Comonad w, Functor f)
               => (forall x . w x -> f x) -> (forall x . w x -> Cofree f x)
memoiseComonad f w = Cofree (extract w) (fmap (memoiseComonad f) (f (duplicate w)))
-- "So that's what a cofree comonad is: it's a type that can be used to memoise all of
--  the states that are accessible from a state in a comonad by pressing its buttons.""

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

data Identity a = Identity a
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Pairing Identity Identity where
  pair f (Identity x) (Identity y) = f x y

data (f :+: g) a = LeftF (f a) | RightF (g a)
instance (Functor f, Functor g) => (Functor (f :+: g)) where
  fmap f (LeftF  x) = LeftF  (fmap f x)
  fmap f (RightF x) = RightF (fmap f x)

data (f :*: g) x = f x :*: g x
instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (x :*: y) = (fmap f x) :*: (fmap f y)

instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (LeftF x)  (a :*: _) = pair p x a
  pair p (RightF x) (_ :*: b) = pair p x b

instance (Pairing f f', Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (a :*: _) (LeftF  x) = pair p a x
  pair p (_ :*: b) (RightF x) = pair p b x

instance Pairing ((->) a) ((,) a) where
   pair p f = uncurry (p . f)

data Free f a = Id a | Free (f (Free f a))

joinFree :: (Functor f) => (Free f (Free f a)) -> Free f a
joinFree (Id x)    = x
joinFree (Free fx) = Free (fmap joinFree fx)

instance (Functor f) => Functor (Free f) where
  fmap f (Id x)    = Id (f x)
  fmap f (Free fx) = Free (fmap (fmap f) fx)

instance (Functor f) => Applicative (Free f) where
  pure  = Id
  (<*>) = undefined

instance (Functor f) => Monad (Free f) where
  return  = Id
  m >>= k = joinFree $ fmap k m

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (Cofree a _) (Id x) = p a x
  pair p (Cofree _ fs) (Free gs) = pair (pair p) fs gs

-- "An element of Free g can be thought of as an expression written in a DSL.
-- So this pairing gives a way to apply a monadic expression to a memoised
-- comonad. In other words, if you think of comonads as machines, monads give
-- a language that can be used to compute something based on the output of
-- the machine."

data UpDown a = Up a | Down a 
  deriving Show

instance Functor UpDown where
  fmap f (Up a)   = Up (f a)
  fmap f (Down a) = Down (f a)

type CofreeComagma a = Cofree UpDown a

collatz :: Integer -> UpDown Integer
collatz n = if even n then Down (n `div` 2) else Up (3*n + 1)

memoisedCollatz :: Integer -> CofreeComagma Integer
memoisedCollatz n = Cofree n (fmap memoisedCollatz (collatz n))

data Two a = Two a a deriving Show
instance Functor Two where
  fmap f (Two x y) = Two (f x) (f y)

instance Pairing UpDown Two where
  pair f (Up   a) (Two b _) = f a b
  pair f (Down a) (Two _ c) = f a c

execute :: Cofree UpDown x -> Free Two (x -> r) -> r
execute w m = pair (\x f -> f x) w m

data Direction = WentUp | WentDown deriving Show

choose :: Free Two Direction
choose = Free (Two (return WentUp) (return WentDown))

ex1 :: Free Two (Integer -> String)
ex1 = do
  x <- choose
  y <- choose
  case (x, y) of
    (WentDown, WentDown) -> return (\z -> "Decreased twice " ++ show z)
    _ -> return show

go1 :: Integer -> String
go1 n = execute (memoisedCollatz n) ex1
