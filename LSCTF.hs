module Main where

import Control.Monad
import Data.Maybe
import Data.List.Split
import Options.Applicative
import Options.Applicative.Builder
import System.Directory
import Text.PrettyPrint (Doc, text, hsep, parens, render, ($+$), space)

import Util

data CTFArgs = CTFArgs { sortsel :: String
                       , filtersel :: Maybe String
                       , paths :: [FilePath]
                       , headersOnly :: Bool
                       }

parseCTFArgs = CTFArgs <$>
      (strOption (short 's' <> help ("Field to sort on (fields: " ++ (unwords (map fst sels)) ++ ")")
              <> metavar "<field>" <> value "cat"))
  <*> (   (fmap (Just . ("cat="++)) $ strOption (short 'c' <> help ("Category to filter on:")))
      <|> ((fmap Just (strOption (short 'f' <> help ("Field to filter for (fields: " ++ (unwords (map fst sels)) ++ ")")
              <> metavar "<field=val>"))) <|> (pure Nothing))
      )
  <*> many (strArgument (metavar "<paths>"))
  <*> switch (short 'g' <> help "Display only group names.")

main = do
  CTFArgs sortsels filtersel mpaths ho <- execParser (info (helper <*> parseCTFArgs) fullDesc)
  let sortsel = fromMaybe cat . flip lookup sels $ sortsels
  paths <- if null mpaths then return ["."] else filterM isDirectory mpaths
  challs <- concat <$> mapM (getChallsForDir <=< canonicalizePath) paths
  putStrLn . render . displayGroupedChalls ho sortsels . groupChallsBy sortsel . processFilter filtersel $ challs
  where
    processFilter Nothing  = id
    processFilter (Just f) = let (sel,val) = (\(x:xs) -> (x,concat xs)) . splitOn "=" $ f
                             in case lookup sel sels of
                                  Just fn -> filter ((==val) . fn)
                                  Nothing -> id

