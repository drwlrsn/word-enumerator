module Lib
  ( getResult
  , Flags(..)
  , countFileWords
  , tab
  ) where

import qualified Data.Text as DT         
import           Data.List                      ( intercalate )
import           Prelude                 hiding ( null, foldl, words )
import           System.IO                      ( Handle
                                                , hGetLine, hGetContents
                                                )
import           System.IO.Error                ( catchIOError
                                                , isEOFError
                                                )

data Flags = Flags
  { optc :: Bool
                     -- "The number of bytes in each input file is written to the standard output.  This will cancel out any prior usage of the -m option."
  , optl :: Bool
                     -- "The number of lines in each input file is written to the standard output."
  , optm :: Bool
                     -- "The number of characters in each input file is written to the standard output.  If the current locale does not support multibyte characters, this is equivalent to the -c option.  This will cancel out any prior usage of the -c option."
  , optw :: Bool -- "The number of words in each input file is written to the standard output."
  }

tab :: String
tab = "       "
getResult
  :: Flags -> Integer -> Integer -> Integer -> Integer -> String -> String
getResult options wordCount lineCount charCount size fileName =
  tab ++ r ++ " " ++ fileName
 where
  r = case options of
    Flags True  False _     False -> show size
    Flags False True  False False -> show lineCount
    Flags False False True  False -> show charCount
    Flags False False False True  -> show wordCount
    Flags True True _ False -> intercalate tab (show <$> [lineCount, size])
    Flags False True True False ->
      intercalate tab (show <$> [lineCount, charCount])
    Flags False True False True ->
      intercalate tab (show <$> [lineCount, wordCount])
    Flags True False False True -> intercalate tab (show <$> [wordCount, size])
    Flags False False True True ->
      intercalate tab (show <$> [wordCount, charCount])
    Flags False True True True ->
      intercalate tab (show <$> [lineCount, wordCount, charCount])
    Flags{} -> intercalate tab (show <$> [lineCount, wordCount, size])


countLineBreaks :: Integer -> Char -> Integer
countLineBreaks n c = if c == '\n' then succ n else n

countFileWords :: Handle -> IO (Integer, Integer, Integer)
countFileWords handle = do
  s <- hGetContents handle
  let text = DT.pack s in
    let wordCount = toInteger $ length $ DT.words text in
      let lineCount =  DT.foldl countLineBreaks 0 text in
        let charCount = DT.length text in
            pure (wordCount, lineCount, toInteger charCount)


