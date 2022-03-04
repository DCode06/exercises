{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
	, check
    ) where 

makeSnippet::Int -> String -> String
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

sumOfSquares:: Int -> Int -> Int
sumOfSquares x y = (x^2) + (y ^2)

lastDigit::Int->Int
lastDigit x = abs $ x `rem` 10

minmax::Int -> Int -> Int-> Int
minmax x y z = let (max,min) = (maximum [x,y,z],minimum [x,y,z]) in (max - min)

subString:: Int -> Int-> String -> String
subString start end str
    | end < 0 || end < start = ""
    | start < 0              = subString 0 end str
    | otherwise              = take (end - start + 1) $drop start str

strSum :: String -> Int
strSum "" = 0
strSum str = sum(map read $ words str ::[Int])

lowerAndGreater:: Int-> [Int]-> String
lowerAndGreater num [] = show num ++" is greater than "++ show 0 ++ " elements and lower than "++ show 0  ++ " elements"
lowerAndGreater num (x:xs) = let cTuple = check num 0 0 (x:xs) in show num ++" is greater than "++ show(fst cTuple) ++ " elements and lower than "++show(snd cTuple) ++ " elements"

check:: Int -> Int -> Int -> [Int]-> (Int,Int)
check i countG countL [] = (countG,countL)
check i countG countL (x:xs)
    | i>x = check i (countG+1) countL xs
    | i<x = check i countG (countL+1) xs
    | otherwise = check i countG countL xs
