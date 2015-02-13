{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Module     : $Header$
Description: clac.
Copyright  : (c) Alexander Berntsen 2015
License    : GPL-3

Maintainer : alexander@plaimi.net

A very simple CLI RPN calculator. Works with STDIN and arguments.

Usage examples:

@
$ clac 1 2 - 3 +
$ echo 1 2 - 3 + | clac
$ clac
  1 1 +^D
@
-}
module Clac where

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
import Data.List
  (
  find,
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
  help,
  long,
  many,
  short,
  strArgument,
  switch,
  )
import Safe
  (
  readMay,
  )


-- |Holds information about the user passed options.
data Opt = MkOpt {wantHelp    :: Bool
                 ,wantVerbose :: Bool
                 ,getEquation :: [String]
                 }

-- |An item on the calculator stack. Can either be a constant number,
-- or an operator.
data StackItem a where
  -- A constant number on the stack, such as '3'.
  StackNum :: forall a. Fractional a => a -> StackItem a
  -- An operator on the stack, such as '+', '-' etc.
  StackOp  :: OpDesc -> StackItem a
deriving instance Show a => Show (StackItem a)

-- |Operator-description binding.
data OpDesc = MkOpDesc {op   :: Op
                       ,desc :: String
                       }

instance Show OpDesc where
  show (MkOpDesc _ a) = a

-- |The valid operator classes.
data Op where
  BinaryOp :: (forall a. Fractional a => a -> a -> a) -> Op
  UnaryOp  :: (forall a. Floating   a =>      a -> a) -> Op
  Constant :: (forall a. Floating   a =>           a) -> Op
  NewEq    :: Op

-- |Parser for the user requested options.
optParser :: Parser Opt
optParser = MkOpt
  <$> switch
      ( long "operations"
     <> short 'o'
     <> help "Print all operations" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output" )
  <*> many (strArgument mempty)

-- |Predefined operator stack. This reflects the current program features.
operators :: [(OpDesc, String)]
operators =
  [( MkOpDesc (BinaryOp (+))   "+",    "+:\t\taddition"                 )
  ,( MkOpDesc (BinaryOp (-))   "-",    "-:\t\tsubtraction"              )
  ,( MkOpDesc (BinaryOp (*))   "*",    "*:\t\tmultiplication"           )
  ,( MkOpDesc (BinaryOp (*))   "x",    "*:\t\tmultiplication"           )
  ,( MkOpDesc (BinaryOp (/))   "/",    "/:\t\tdivision"                 )
  ,( MkOpDesc (UnaryOp negate) "neg",  "neg:\t\tnegation"               )
  ,( MkOpDesc (UnaryOp sin)    "sin",  "sin:\t\tsine function"          )
  ,( MkOpDesc (UnaryOp cos)    "cos",  "cos:\t\tcosine function"        )
  ,( MkOpDesc (UnaryOp tan)    "tan",  "tan:\t\ttangent function"       )
  ,( MkOpDesc (UnaryOp asin)   "asin", "asine:\t\tarcsine function"     )
  ,( MkOpDesc (UnaryOp acos)   "acos", "acosine:\tarccosine function"   )
  ,( MkOpDesc (UnaryOp atan)   "atan", "arctan:\t\tarctangent function" )
  ,( MkOpDesc (Constant   pi)  "pi",   "pi:\t\tpi constant"             )
  ,( MkOpDesc NewEq            ",",    ",:\t\tstart a new equation"     )
  ]

-- |Build the calculator stack. Usually used in conjunction with foldr,
-- to build the complete stack consecutively.
buildStack :: String             -- ^ one equation item
           -> [StackItem Double] -- ^ prepend the result to this stack
           -> [StackItem Double]
buildStack str ac = case parseStack str of
                      Just q  -> q:ac
                      Nothing -> ac

-- |Parse an equation item.
parseStack :: String                   -- ^ one equation item
           -> Maybe (StackItem Double) -- ^ converted stack item, if valid
parseStack str = (StackOp <$> find ((== str) . desc) (fst <$> operators))
                 <|> StackNum <$> (readMay str :: Maybe Double)

-- |Build a tree from a calculator stack, suitable for pretty printing.
stackTree :: [StackItem Double] -> Tree String
stackTree (StackOp (MkOpDesc (BinaryOp _) i):j:k) =
  Node i [stackTree k, stackTree [j]]
stackTree (StackOp (MkOpDesc (UnaryOp _) i):j) =
  Node i [stackTree j]
stackTree (StackOp (MkOpDesc (Constant _  ) i):_) =
  Node i []
stackTree (StackNum i:_) =
  Node (show i) []
stackTree _  =
  Node "¯\\_(ツ)_/¯" []

-- |Solve a stack.
solveStack :: [StackItem Double]  -- ^ stack to solve
           -> [StackItem Double]  -- ^ stack used for zipping, initially empty
           -> Maybe Double        -- ^ calculation result
solveStack (StackOp (MkOpDesc (BinaryOp o) _):ss) (StackNum n:StackNum m:ts) =
  solveStack ss (StackNum (m `o` n):ts)
solveStack (StackOp (MkOpDesc (UnaryOp o) _):ss) (StackNum m:ts) =
  solveStack ss (StackNum (o m):ts)
solveStack (StackOp (MkOpDesc (Constant   c) _):ss) ts =
  solveStack ss (StackNum c:ts)
solveStack (n:ss) ts =
  solveStack ss (n:ts)
solveStack [] (StackNum n:_) =
  Just n
solveStack _ _ =
  Nothing

-- |Solve multiple equations of an input list such as:
--
-- > [["1","1","+"],["2","2","+"]]
--
-- This gives back a tuple for each equation where the first value
-- is the result of the equation (if any) and the second one
-- is a String representing the stackTree, suitable for printing.
solveAll :: [[String]] -> [(Maybe Double, String)]
solveAll =
  map
  $ (second drawVerticalTree . (((,) . flip solveStack [])
    <*> (stackTree . reverse)))
  . foldr buildStack []


