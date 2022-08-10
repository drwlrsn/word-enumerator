module Lib (getResult, Flags(Flags), countFileWords) where

import           Data.List (intercalate)
import           Data.Text (Text, split, pack, unpack, null)
import           System.IO (Handle, hGetLine)
import           System.IO.Error (catchIOError, isEOFError)
import           Prelude hiding (null)

data Flags =
  Flags { optc :: Bool
          -- "The number of bytes in each input file is written to the standard output.  This will cancel out any prior usage of the -m option."
        , optl :: Bool
          -- "The number of lines in each input file is written to the standard output."
        , optm :: Bool
          -- "The number of characters in each input file is written to the standard output.  If the current locale does not support multibyte characters, this is equivalent to the -c option.  This will cancel out any prior usage of the -c option."
        , optw :: Bool -- "The number of words in each input file is written to the standard output."
        }

tab :: [Char]
tab = "       "
getResult
  :: Flags -> Integer -> Integer -> Integer -> Integer -> String -> String
getResult options wordCount lineCount charCount size fileName =
  tab ++ r ++ " " ++ fileName
  where
    r = case options of
      Flags True False False False -> show size
      Flags False True False False -> show lineCount
      Flags False False True False -> show charCount
      Flags False False False True -> show wordCount
      Flags True True False False -> intercalate tab (show <$> [lineCount, size]) 
      Flags {} -> intercalate tab (show <$> [lineCount, wordCount, size])

splitOnSpace :: Text -> [Text]
splitOnSpace = split (\c -> c == ' ' || c == '\t')

countLineWords :: Text -> Int
countLineWords = length . splitOnSpace

countFileWords :: Int -> Int -> Int -> Handle -> IO (Int, Int, Int)
countFileWords wordCount lineCount charCount handle = do
  line <- pack
    <$> catchIOError
      (hGetLine handle)
      (\e -> if isEOFError e
             then return ""
             else ioError e)
  if null line
    then pure (wordCount, lineCount, charCount)
    else countFileWords
      (wordCount + countLineWords line)
      (succ lineCount)
      (succ $ length $ unpack line) -- add one for the line break
      handle

