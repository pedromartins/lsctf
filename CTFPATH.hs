module Main where

import Options.Applicative
import Options.Applicative.Builder
import System.Directory

import Util

main = do
  challn <- execParser (info (helper <*> strArgument (metavar "<chall>")) fullDesc)
  challs <- getChallsForDir =<< canonicalizePath "."
  putStrLn . path . head $ filter ((==challn) . chall) challs

