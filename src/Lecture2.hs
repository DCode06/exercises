{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}
module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , DragonType (..)
    , Dragon (..)
    , TreasureChest (..)
    , FightOutcome (..)
    , dragonFight
    , calculateDragonHealth
    , calculateKnightHealth
    , calculateKnightEndurance

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , State
    , eval
    , combine
    , constantFolding
    , constrExpr
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Either

lazyProduct :: [Int] -> Int
lazyProduct [] = 1
lazyProduct (0 : _) = 0
lazyProduct (x : xs) = x * lazyProduct xs

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = x : x : duplicate xs

removeAt::Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing,[])
removeAt 0 (x : xs) = (Just x, xs)
removeAt n (x : xs)
    | n > 0 = let (v, rem) = removeAt (n -1) xs in (v, x : rem)
	| otherwise = (Nothing,(x : xs))

evenLists::[[a]] -> [[a]]
evenLists [] = []
evenLists [[]] = [[]]
evenLists ((x:xs):tx) = [(x:xs) | (x:xs) <- ((x:xs):tx),  (even.length) (x:xs)]

dropSpaces :: String -> String
dropSpaces str = takeWhile (not.isSpace) $ dropWhile isSpace str

data Knight = Knight
    { knightHealth    :: Int
    , knightAttack    :: Int
    , knightEndurance :: Int
    }

data DragonType = Red| Black |Green

data Dragon = Dragon
    { dragonType :: DragonType
	, dragonHealth :: Int
	, dragonFirePower :: Int
	, treasureChest :: Maybe TreasureChest 
	, expPoints :: Int}

data TreasureChest = TreasureChest
    { gold :: Int
	, treasure :: Int}

data FightOutcome = DragonDead | KnightDead | KnightRunsAway | DragonAlive

dragonFight:: Dragon -> Knight -> FightOutcome
dragonFight d k = let dH = calculateDragonHealth (dragonHealth d) (knightAttack k)
                      kH = calculateKnightHealth (dragonFirePower d) (knightHealth k)
                      kE = calculateKnightEndurance (knightAttack k) (knightEndurance k)
				  in case (dH,kH,kE) of
				         (dH, _,_) | dH<= 0-> DragonDead
				         (dH, _,_) | dH> 0-> DragonAlive
				         (_,kH,_)  |kH<=0-> KnightDead
				         (_,_,kE)  |kE==0 -> KnightRunsAway

calculateDragonHealth::Int -> Int -> Int
calculateDragonHealth dragonHealth knightAttack = dragonHealth - knightAttack

calculateKnightHealth::Int -> Int -> Int
calculateKnightHealth dragonFirePower knightHealth = knightHealth - dragonFirePower

calculateKnightEndurance::Int -> Int -> Int
calculateKnightEndurance knightAttack knightEndurance = knightEndurance - knightAttack

isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [x] = True
isIncreasing (x : y : ys) = x < y && isIncreasing (y : ys)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) = if (x < y) then x : merge xs (y : ys) else y : merge (x : xs) ys

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where left = take len xs;
          right = drop len xs;
		  len = length xs `div` 2

data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

type Variables = [(String, Int)]

data EvalError
    = VariableNotFound String
    deriving (Show, Eq)


eval :: Variables -> Expr -> Either EvalError Int
eval var (Lit n) = Right n
eval var (Var str) = case lookup str var of
                        Nothing -> Left (VariableNotFound str)
                        Just n  -> Right n
eval var (Add e f) = combine ( if (isLeft $ eval var e) then Left $ fromLeft (VariableNotFound "s") (eval var e) else Right $ fromRight 1 (eval var e)) (if (isLeft $ eval var f) then Left $ fromLeft (VariableNotFound "s") (eval var f) else Right $ fromRight 1 (eval var f))

combine:: Either EvalError Int -> Either EvalError Int -> Either EvalError Int
combine (Left (VariableNotFound str)) _ = Left (VariableNotFound str)
combine _ (Left (VariableNotFound str)) = Left (VariableNotFound str)
combine (Right a) (Right b) = Right (a+b)

constantFolding :: Expr -> Expr
constantFolding expr = do
  let (res, constSum) = constrExpr expr
  case res of
    Nothing -> Lit constSum
    Just (Var str) -> Var str
    Just exp -> Add exp (Lit constSum)

type State = Int

constrExpr :: Expr -> (Maybe Expr, State)
constrExpr expr =
  case expr of
    Lit x -> (Nothing, x)
    Var str -> (Just (Var str), 0)
    Add ep1 ep2 -> do
      let (re1, s1) = constrExpr ep1
      let (re2, s2) = constrExpr ep2

      case (re1, re2) of
        (Nothing, x) -> (x, s1 + s2)
        (x, Nothing) -> (x, s1 + s2)
        (Just e1, Just e2) -> (Just (Add e1 e2), s1 + s2)
