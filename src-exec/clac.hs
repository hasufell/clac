{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Module     : $Header$
Description: clac.
Copyright  : (c) Alexander Berntsen 2015
License    : GPL-3

Maintainer : alexander@plaimi.net
-} module Main where

import Control.Applicative
  (
  (<$>),
  (<|>),
  (<*>),
  )
import Control.Arrow
  (
  second,
  )
import Control.Monad
  (
  when,
  )
import Data.List
  (
  find,
  )
import Data.List.Split
  (
  splitOn,
  )
import Data.Monoid
  (
  (<>),
  mempty,
  )
import Data.Tree
  (
  Tree (Node),
  )
import Data.Tree.Pretty
  (
  drawVerticalTree,
  )
import Options.Applicative
  (
  Parser,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  many,
  progDesc,
  short,
  strArgument,
  switch,
  )
import Safe
  (
  readMay,
  )


data Opt = MkOpt {h :: Bool
                 ,v :: Bool
                 ,e :: [String]
                 }

data StackItem a where
  Snum :: forall a. Fractional a => a -> StackItem a
  Sop  :: OpDesc -> StackItem a
deriving instance Show a => Show (StackItem a)

data OpDesc = Dop {op   :: Op
                  ,desc :: String
                  }
instance Show OpDesc where
  show (Dop _ a) = a

data Op where
  Bop :: (forall a. Fractional a => a -> a -> a) -> Op
  Uop :: (forall a. Floating a => a -> a) -> Op
  C   :: (forall a. Floating a => a) -> Op
  Neq :: Op

ops :: Parser Opt
ops = MkOpt
  <$> switch
      ( long "operations"
     <> short 'o'
     <> help "Print all operations" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output" )
  <*> many (strArgument mempty)

os :: [(OpDesc, String)]
os = [( Dop (Bop (+))    "+",   "+:\t\taddition"                 )
     ,( Dop (Bop (-))    "-",   "-:\t\tsubtraction"              )
     ,( Dop (Bop (*))    "*",   "*:\t\tmultiplication"           )
     ,( Dop (Bop (*))    "x",   "*:\t\tmultiplication"           )
     ,( Dop (Bop (/))    "/",   "/:\t\tdivision"                 )
     ,( Dop (Uop negate) "neg", "neg:\t\tnegation"               )
     ,( Dop (Uop sin)    "sin", "sin:\t\tsine function"          )
     ,( Dop (Uop cos)    "cos", "cos:\t\tcosine function"        )
     ,( Dop (Uop tan)    "tan", "tan:\t\ttangent function"       )
     ,( Dop (Uop asin)   "asin","asine:\t\tarcsine function"     )
     ,( Dop (Uop acos)   "acos","acosine:\tarccosine function"   )
     ,( Dop (Uop atan)   "atan","arctan:\t\tarctangent function" )
     ,( Dop (C   pi)     "pi",  "pi:\t\tpi constant"             )
     ,( Dop Neq          ",",   ",:\t\tstart a new equation"     )
     ]

b :: String -> [StackItem Double] -> [StackItem Double]
b x ac = case p x of
           Just q  -> q:ac
           Nothing -> ac

p :: String -> Maybe (StackItem Double)
p m = (Sop <$> find ((== m) . desc) (fst <$> os))
  <|> Snum <$> (readMay m :: Maybe Double)

t :: [StackItem Double] -> Tree String
t (Sop (Dop (Bop _) i):j:k) = Node i [t k, t [j]]
t (Sop (Dop (Uop _) i):j)   = Node i [t j]
t (Sop (Dop (C _  ) i):_)   = Node i []
t (Snum i:_)                = Node (show i) []
t _                         = Node "¯\\_(ツ)_/¯" []

s :: [StackItem Double] -> [StackItem Double] -> Maybe Double
s (Sop (Dop (Bop o) _):ss) (Snum n:Snum m:ts) = s ss (Snum (m `o` n):ts)
s (Sop (Dop (Uop o) _):ss) (Snum m:ts)        = s ss (Snum (o m):ts)
s (Sop (Dop (C   c) _):ss) ts                 = s ss (Snum c:ts)
s (n:ss) ts                                   = s ss (n:ts)
s [] (Snum n:_)                               = Just n
s _ _                                         = Nothing

sa :: [[String]] -> [(Maybe Double, String)]
sa = map $ (second drawVerticalTree . (((,) . flip s [])
 <*> (t . reverse))) . foldr b []

calc :: Opt -> IO ()
calc o = do
  cs <- if null (e o) then getContents else return []
  let es = splitOn [","] $ words cs ++ case e o of
                                         [a] -> words a
                                         _   -> e o
  if h o
    then mapM_ putStrLn $ "OPERATORS":"=========":map snd os
    else
      mapM_ (\(solution, tree) -> do
        when (v o) $ putStrLn $ "\n\n" ++ tree
        print solution
        putStrLn $ replicate (length $ show solution) '=') $ sa es

main :: IO ()
main = execParser o >>= calc
  where o =
          info (helper <*> ops)
          ( fullDesc
         <> progDesc "simple CLI RPN calculator"
         <> header   "clac" )
