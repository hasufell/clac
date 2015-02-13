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


data Opt = MkOpt {wantHelp    :: Bool
                 ,wantVerbose :: Bool
                 ,getEquation :: [String]
                 }

data StackItem a where
  StackNum :: forall a. Fractional a => a -> StackItem a
  StackOp  :: OpDesc -> StackItem a
deriving instance Show a => Show (StackItem a)

data OpDesc = MkOpDesc {op   :: Op
                       ,desc :: String
                       }

instance Show OpDesc where
  show (MkOpDesc _ a) = a

data Op where
  BinaryOp :: (forall a. Fractional a => a -> a -> a) -> Op
  UnaryOp  :: (forall a. Floating   a =>      a -> a) -> Op
  Constant :: (forall a. Floating   a =>           a) -> Op
  NewEq    :: Op

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

buildStack :: String -> [StackItem Double] -> [StackItem Double]
buildStack str ac = case parseStack str of
                      Just q  -> q:ac
                      Nothing -> ac

parseStack :: String -> Maybe (StackItem Double)
parseStack str = (StackOp <$> find ((== str) . desc) (fst <$> operators))
                 <|> StackNum <$> (readMay str :: Maybe Double)

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

solveStack :: [StackItem Double] -> [StackItem Double] -> Maybe Double
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

solveAll :: [[String]] -> [(Maybe Double, String)]
solveAll =
  map
  $ (second drawVerticalTree . (((,) . flip solveStack [])
    <*> (stackTree . reverse)))
  . foldr buildStack []

calc :: Opt -> IO ()
calc opt = do
  cs <- if null (getEquation opt) then getContents else return []
  let es = splitOn [","] $ words cs ++ case getEquation opt of
                                         [a] -> words a
                                         _   -> getEquation opt
  if wantHelp opt
    then mapM_ putStrLn $ "OPERATORS":"=========":map snd operators
    else
      mapM_ (\(solution, tree) -> do
        when (wantVerbose opt) $ putStrLn $ "\n\n" ++ tree
        print solution
        putStrLn $ replicate (length $ show solution) '=') $ solveAll es

main :: IO ()
main = execParser o >>= calc
  where o =
          info (helper <*> optParser)
          ( fullDesc
          <> progDesc "simple CLI RPN calculator"
          <> header   "clac" )
