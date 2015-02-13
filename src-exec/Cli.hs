module Main where

import Clac
import Control.Applicative
  (
  (<*>),
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
