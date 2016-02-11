{-# LANGUAGE ScopedTypeVariables #-}
module Util where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Monoid
import System.Console.ANSI
import System.Directory
import System.FilePath
import Text.PrettyPrint (Doc, text, hsep, parens, render, ($+$), space)
import Data.Maybe
import Text.Regex
import Text.Regex.Posix
import Safe
import Control.Exception

data CTF = CTF { name :: String, challs :: [Chall] }
  deriving (Eq, Show)

data Chall = Chall { chall :: String
                   , cat :: String
                   , score :: Maybe Integer
                   , path :: FilePath
                   , desc :: String
                   , solved :: Solved
                   }
  deriving (Eq, Show)

data Solved = Yes | No | Broken
  deriving (Eq, Show)

isDirectory :: FilePath -> IO Bool
isDirectory = fmap not . doesFileExist

-- getCTFForDir :: FilePath -> IO [CTF]
-- getCTFForDir fp = do
--   nodes <- listDirectory fp
--   ctfs <- filterM isDirectory nodes
--   forM ctfs $ \ctf -> do
--     ctfchalls <- getChallsForDir (fp </> ctf)
--     return $ CTF { name = ctf, challs = ctfchalls }

getChallsForDir :: FilePath -> IO [Chall]
getChallsForDir fp = do
  nodes <- listDirectory fp
  cats <- filterM isDirectory . map (fp </>) $ nodes
  fmap concat . forM cats $ \cat -> do
    catnodes <- listDirectory (fp </> cat)
    challs <- filterM isDirectory . map ((fp </> cat)</>) $ catnodes
    if null challs
       then fmap (:[]) $ getChall Nothing (fp </> cat)
       else mapM (getChall (Just $ takeFileName cat) . (\c -> fp </> cat </> c)) challs

scrapeField :: String -> [String] -> Maybe String
scrapeField f = fmap (map toLower) . listToMaybe . concatMap (take 1 . drop 1 . splitOn " ")
                . filter (matchTest (mkRegexWithOpts f True False))

getChall :: Maybe String -> FilePath -> IO Chall
getChall cat' fp = do
  nodes <- listDirectory fp
  let descFile = listToMaybe . take 1 . filter ((=="readme.md") . map toLower) $ nodes
      flagFile = listToMaybe . take 1 . filter ((=="flag.txt") . map toLower) $ nodes
      brokenFile = listToMaybe . take 1 . filter ((=="broken") . map toLower) $ nodes

  challDesc <- case descFile of
                 Nothing -> return ""
                 Just x -> readFile (fp </> x)
  let cl = lines challDesc
      challBits = splitOn "-" . takeFileName $ fp
      challScore = take 1 . reverse $ challBits
  fcat <- handle (\(e :: SomeException) -> return Nothing) . evaluate
            . getFirst $ (  First cat'
                        <> First (fmap (takeWhile (/=',')) . scrapeField "category" $ cl)
                        <> First (Just . intercalate "-" . init $ challBits))
  fscore <- handle (\(e :: SomeException) -> return Nothing) . evaluate
              . getFirst $ First (join . fmap readMay . scrapeField "(score)|(points)" $ cl)
                         <> First (join . fmap readMay . listToMaybe $ challScore)
  return Chall { chall = takeFileName fp
               , cat = fromMaybe "unknown" fcat
               , score = fscore
               , path = fp
               , desc = challDesc
               , solved = case flagFile of
                            Nothing -> case brokenFile of
                                        Nothing -> No
                                        Just _  -> Broken
                            Just _  -> Yes
               }

groupChallsBy :: (Ord a) => (Chall -> a) -> [Chall] -> [(a, [Chall])]
groupChallsBy sel =
  map (\cs -> (sel . head $ cs, cs)) . groupBy ((==) `on` sel) . sortOn sel


displayChall :: String -> Chall -> Doc
displayChall exc c =
 let solvedDoc = case solved c of
                   Yes -> text "\x1b[32m✓\x1b[0m"
                   No -> text "\x1b[31m✗\x1b[0m"
                   Broken -> text "💣"
 in hsep [ text (ex "chall" $ chall c)
         , ex "cat" . parens . text $ cat c
         , ex "score" . parens . maybeText . fmap show $ score c
         , solvedDoc]
  where ex c d = if exc == c then mempty else d
        maybeText :: Maybe String -> Doc
        maybeText Nothing = mempty
        maybeText (Just s) = text s

displayGroupedChalls :: Bool -> String -> [(String, [Chall])] -> Doc
displayGroupedChalls ho c =
  foldl1 (\x1 x2 -> x1 $+$ space $+$ x2)
  . map (\(x,cs) -> text ("\x1b[37;1m" ++ x ++ ":\x1b[0m")
                      $+$ if ho then mempty else (foldl1 ($+$) . map (displayChall c) $ cs))

sels :: [ (String, Chall -> String) ]
sels = [ ("chall", chall)
       , ("cat", cat)
       , ("score", maybe "" show . score)
       , ("path", path)
       , ("desc", desc)
       , ("solved", show . solved)
       ]
