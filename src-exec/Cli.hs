module Main where

import Clac
import Control.Applicative
  (
  (<*>),
  )
import Control.Monad
  (
  when,
  )
import Data.List.Split
  (
  splitOn,
  )
import Data.Monoid
  (
  (<>),
  )
import Options.Applicative
  (
  execParser,
  fullDesc,
  header,
  helper,
  info,
  progDesc,
  )

main :: IO ()
main = execParser o >>= calc
  where o =
          info (helper <*> optParser)
          ( fullDesc
          <> progDesc "simple CLI RPN calculator"
          <> header   "clac" )

-- |Run the calculator with the given options.
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
