{-# LANGUAGE RankNTypes #-} -- for interpretMonad
module Main where

import Control.Applicative
import Control.Monad (join)
import Control.Monad.Reader
import Control.Monad.State
import Test.QuickCheck

-- * Magma

class Magma m where
  o :: m -> m -> m

instance Magma Int where
 o = (+)

accumulate :: (Magma m) => m -> [m] -> m
accumulate a []     = a
accumulate a (b:bs) = accumulate (a `o` b) bs

accumulate' :: (Magma m) => m -> [m] -> m
accumulate' a []     = a
accumulate' a (b:bs) = accumulate' (b `o` a) bs

data FreeMagma a
  = Var a
  | Tree (FreeMagma a) (FreeMagma a)
  deriving (Eq, Show)

instance Magma (FreeMagma a) where
 o = Tree

interpretMagma :: (Magma b) => (a -> b) -> (FreeMagma a -> b)
interpretMagma f (Var a) = f a
interpretMagma f (Tree a b) = (interpretMagma f a) `o` (interpretMagma f b)

x :: FreeMagma String
x = Var "a" `o` (Var "a" `o` Var "b")

fx :: String -> Int
fx "a" = 1
fx "b" = 2

-- * Monoid

class Monoid m where
  (<>) :: m -> m -> m
  mempty :: m

momoidAssociativityLaw :: (Eq m, Monoid m) => m -> m -> m -> Bool
momoidAssociativityLaw a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentityLaw :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentityLaw a = a <> mempty == a

monoidRightIdentityLaw :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentityLaw b = mempty <> b == b

instance Monoid Int where
  (<>) = (+)
  mempty = 0

-- We suspect this could be a free monoid, but it is not true, check out the laws
data FM a
  = FM_Var a
  | FM_Mempty
  | FM_Tree (FM a) (FM a)
  deriving (Eq, Show)

instance Monoid (FM a) where
  mempty = FM_Mempty
  (<>) = FM_Tree

instance (Arbitrary a) => Arbitrary (FM a) where
  arbitrary = sized freeMonoid
    where
      freeMonoid 0 = oneof [return FM_Mempty, FM_Var <$> arbitrary]
      freeMonoid n = oneof [return FM_Mempty, FM_Tree <$> subFreeMonoid <*> subFreeMonoid]
        where
          subFreeMonoid = freeMonoid (n `div` 2)

-- List is the free monoid!
type FreeMonoid = []

instance Monoid ([] a) where
 (<>) = (++)
 mempty = []

interpretMonoid :: (Monoid b) => (a -> b) -> ([a] -> b)
interpretMonoid f []     = mempty
interpretMonoid f (a:as) = f a <> interpretMonoid f as

-- Simple interpretation examples

data BankOp = Deposit Int | Withdraw Int

interpretBankOp :: BankOp -> Int
interpretBankOp (Deposit d) = d
interpretBankOp (Withdraw w) = -w

bankOpProgram = [Deposit 10, Withdraw 5, Withdraw 5, Deposit 6]

-- * Free Monads from free monoid analogy

-- Recursive type equatation of X = 1 + a * X
data List a = Nil | Cons a (List a)

-- Recursive type equatation of X = e + f X
data Free f a = Id a | Free (f (Free f a))

join' :: (Functor f) => Free f (Free f a) -> Free f a
join' (Id x)    = x
join' (Free fa) = Free (fmap join' fa)

instance (Functor f) => Functor (Free f) where
  fmap f (Id x)    = Id (f x)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

instance (Functor f) => Applicative (Free f) where
  pure  = Id
  (<*>) = undefined -- TODO: Monad-Applicative proposal

instance (Functor f) => Monad (Free f) where
  return  = Id
  m >>= k = join' (fmap k m)

interpretMonad :: (Functor a, Functor b, Monad b) => (forall x . a x -> b x) -> (forall x . Free a x -> b x)
interpretMonad f (Id x)    = return x
interpretMonad f (Free fa) = join (f (fmap (interpretMonad f) fa))

-- Choice example

data Choice x = Choice x x
  deriving (Eq, Show)

instance Functor Choice where
  fmap f (Choice a b) = Choice (f a) (f b)

choice :: Free Choice Bool
choice = Free (Choice (Id True) (Id False))

testChoice :: Free Choice String 
testChoice = do 
 a <- choice 
 b <- choice 
 return $ if a && b
   then "Both true"
   else "At least one false"

inter1 :: Choice x -> IO x
inter1 (Choice a b) = do
  c <- readLn
  return $ if c then a else b

go1 = interpretMonad inter1 testChoice

inter2 :: Choice x -> Reader Bool x
inter2 (Choice a b) = do
  c <- ask
  return $ if c then a else b

go2 = runReader (interpretMonad inter2 testChoice) True

inter3 :: Choice x -> State [Bool] x
inter3 (Choice a b) = do
  x:xs <- get
  put xs
  return $ if x then a else b

go3 = evalState (interpretMonad inter3 testChoice) [True, False, True]

-- * Main

main = do
  test "Accum #1" (accumulate 0  [1, 2, 3])  (6 :: Int)
  test "Accum #2" (accumulate 10 [(-10), 5]) (5 :: Int)
  test "Accum #1" (accumulate 0  [1, 2, 3])  (6 :: Int)
  test "Accum #2" (accumulate 10 [(-10), 5]) (5 :: Int)
  test "Free Magma #1" (interpretMagma fx x) (4 :: Int)
  lawTest "Int monoid associativity" (momoidAssociativityLaw <$> arbitrary <*> arbitrary <*> arbitraryInt)
  lawTest "Int monoid left identity" (monoidLeftIdentityLaw <$> arbitraryInt)
  lawTest "Int monoid right identity" (monoidRightIdentityLaw <$> arbitraryInt)
  lawTest "FM monoid associativity" (momoidAssociativityLaw <$> arbitrary <*> arbitrary <*> arbitraryFM0)
  lawTest "FM monoid left identity" (monoidLeftIdentityLaw <$> arbitraryFM0)
  lawTest "FM monoid right identity" (monoidRightIdentityLaw <$> arbitraryFM0)
  lawTest "FM monoid associativity" (momoidAssociativityLaw <$> arbitrary <*> arbitrary <*> arbitraryFreeMonoid0)
  lawTest "FM monoid left identity" (monoidLeftIdentityLaw <$> arbitraryFreeMonoid0)
  lawTest "FM monoid right identity" (monoidRightIdentityLaw <$> arbitraryFreeMonoid0)
  test "BankOp program" (interpretMonoid interpretBankOp bankOpProgram) 6
  -- go1
  print go2
  print go3

-- * Test helpers

test :: (Eq a) => String -> a -> a -> IO ()
test name x y = do
  putStrLn $ concat [name, " ", if x == y then "passed" else "failed", "."]

lawTest name lawProp = do
  putStr (name ++ " ")
  quickCheck lawProp

arbitraryInt :: Gen Int
arbitraryInt = arbitrary

arbitraryFM0 :: Gen (FM ())
arbitraryFM0 = arbitrary

arbitraryFreeMonoid0 :: Gen [()]
arbitraryFreeMonoid0 = arbitrary
