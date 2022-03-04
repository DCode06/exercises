{-# LANGUAGE InstanceSigs #-}

{- |
Module                  : Lecture3
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 3 of the Haskell Beginners course.

In this module you're going to practice standard Haskell typeclasses:

  * Deriving instances
  * Using typeclasses methods
  * Implementing instances manually
  * Becoming friends with Semigroup, Monoid, Foldable and Functor typeclasses!

-}

module Lecture3
    ( Weekday (..)
    , toShortString
    , next
    , daysTo
    , findSucc

    , Gold (..)
    , Reward (..)
    , List1 (..)
    , Treasure (..)

    , appendDiff3
    , apply
    ) where


-- $setup
import Data.Semigroup
import Data.List

data Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Show, Eq, Enum, Bounded)

toShortString:: Weekday -> String
toShortString day = take 3 $ show day

next :: Weekday -> Weekday
next w = findSucc w

daysTo::Weekday -> Weekday -> Int
daysTo w1 w2 = foldr (\_ y -> y+1) 0 (dayList [w1] w2) 
               where dayList (l1:ls) l2 = if findSucc l1 == l2 then (l1:ls) else dayList((findSucc l1):(l1:ls)) l2

findSucc:: Weekday -> Weekday
findSucc w1
    | w1 == maxBound = minBound
	| otherwise = succ w1

newtype Gold = Gold
    { unGold :: Int
    } deriving (Show, Eq)

instance Semigroup Gold where
    Gold x <> Gold y = Gold (x+y)
	
instance Monoid Gold where
    mempty = Gold 0

data Reward = Reward
    { rewardGold    :: Gold
    , rewardSpecial :: Bool
    } deriving (Show, Eq)

instance Semigroup Reward where
    Reward x s1 <> Reward y s2 = Reward (x <> y) (s1 || s2)

instance Monoid Reward where
    mempty = Reward (Gold 0) False

data List1 a = List1 a [a]
    deriving (Show, Eq)

instance Semigroup(List1 a) where
    (List1 l1 a1)<> (List1 l2 a2)= List1 l1 (a1 ++[l2] ++ a2)


{- | Does 'List1' have the 'Monoid' instance? If no then why?

instance Monoid (List1 a) where
-}
--Listl cannot have a Monoid instance since it cannot have zero elements

data Treasure a
    = NoTreasure
    | SomeTreasure a
    deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Treasure a) where
    NoTreasure <> SomeTreasure a = SomeTreasure a
    NoTreasure <> NoTreasure = NoTreasure
    SomeTreasure a <> NoTreasure = SomeTreasure a
    SomeTreasure a1 <> SomeTreasure a2 = SomeTreasure (a1 <> a2)

instance (Semigroup a) => Monoid (Treasure a) where
    mempty = NoTreasure

appendDiff3:: Eq a => Semigroup a => Monoid a => a -> a-> a-> a
appendDiff3 a1 a2 a3 = foldr mappend mempty $ nub [a1,a2,a3]

-- instance Foldable Weekday where
-- cannot implement Foldable instance for Weekday since expected kind is *-> * but Weekday has kind *
-- instance Foldable Gold where
-- cannot implement Foldable instance for Gold since expected kind is *-> * but Gold has kind *
-- instance Foldable Reward where
-- cannot implement Foldable instance for Reward since expected kind is *-> * but Reward has kind *
instance Foldable List1 where
   foldMap f (List1 l1 l2) = (f l1) `mappend` (foldMap f l2)

instance Foldable Treasure where
    foldr f x NoTreasure = x
    foldr f x (SomeTreasure a) = f a x
    foldMap f (NoTreasure) = mempty
    foldMap f (SomeTreasure a) = f a

-- instance Functor Weekday where
-- cannot implement Functor instance for Weekday since expected kind is *-> * but Weekday has kind *
-- instance Functor Gold where
-- cannot implement Functor instance for Gold since expected kind is *-> * but Gold has kind *
-- instance Functor Reward where
-- cannot implement Functor instance for Reward since expected kind is *-> * but Reward has kind *
instance Functor List1 where
    fmap f (List1 a1 a2) = List1 (f a1) (map f a2)

instance Functor Treasure where
    fmap f NoTreasure = NoTreasure
    fmap f (SomeTreasure a) = SomeTreasure (f a)

apply :: (Functor f) => a -> f (a-> b) -> f b
apply a = fmap (\f -> f a)
