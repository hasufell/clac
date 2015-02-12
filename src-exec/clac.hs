{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Module      :  $Header$
Description :  clac.
Copyright   :  (c) Alexander Berntsen 2015
License     :  GPL-3

Maintainer  :  alexander@plaimi.net
-} module Main where

import Control.Applicative
  (
  (<|>),
  (<$>),
  )
import Safe
  (
  readMay,
  )
import System.Environment
  (
  getArgs,
  )

import Plailude


data StackItem a where
  Snum :: forall a. Fractional a => a -> StackItem a
  Sop  :: Op -> StackItem a
deriving instance Show a => Show (StackItem a)

data Op where
  Bop :: (forall a. Fractional a => a -> a -> a) -> Op
  Uop :: (forall a. Floating a => a -> a) -> Op
instance Show Op where
  show (Bop _) = "binary operator"
  show (Uop _) = "unary operator"

os :: [(String, (StackItem Double, String))]
os = [ ( "+",    ( Sop (Bop (+)),    "+:\t\taddition"                 ))
     , ( "-",    ( Sop (Bop (-)),    "-:\t\tsubtraction"              ))
     , ( "*",    ( Sop (Bop (*)),    "*:\t\tmultiplication"           ))
     , ( "x",    ( Sop (Bop (*)),    "*:\t\tmultiplication"           ))
     , ( "/",    ( Sop (Bop (/)),    "/:\t\tdivision"                 ))
     , ( "neg",  ( Sop (Uop negate), "neg:\t\tnegation"               ))
     , ( "sin",  ( Sop (Uop sin),    "sin:\t\tsine function"          ))
     , ( "cos",  ( Sop (Uop cos),    "cos:\t\tcosine function"        ))
     , ( "tan",  ( Sop (Uop tan),    "tan:\t\ttangent function"       ))
     , ( "asin", ( Sop (Uop asin),   "asine:\t\tarcsine function"     ))
     , ( "acos", ( Sop (Uop acos),   "acosine:\tarccosine function"   ))
     , ( "atan", ( Sop (Uop atan),   "arctan:\t\tarctangent function" ))
     ]

b :: String -> [StackItem Double] -> [StackItem Double]
b x ac = case p x of
           Just q  -> q : ac
           Nothing -> ac

p :: String -> Maybe (StackItem Double)
p m = (fmap fst .: lookup) m os <|> Snum <$> (readMay m :: Maybe Double)

s :: [StackItem Double] -> [StackItem Double] -> Maybe Double
s (Sop (Bop o) : ss) (Snum n : Snum m : ts) = s ss (Snum (m `o` n) : ts)
s (Sop (Uop o) : ss) (Snum m : ts)          = s ss (Snum (o m) : ts)
s (n:ss) ts                                 = s ss (n : ts)
s [] (Snum n:_)                             = Just n
s _ _                                       = Nothing

main :: IO ()
main = do
  as <- getArgs
  bs <- if null as then getContents else return []
  if "help" `elem` as
    then mapM_ putStrLn $ "OPERATORS" : "=========" : [h | (_, (_, h)) <- os]
    else print . flip s [] . foldr b [] $ words bs ++ case as of
                                                        [a] -> words a
                                                        _   -> as
