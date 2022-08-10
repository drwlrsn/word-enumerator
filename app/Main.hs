{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import           Data.Semigroup                 ( (<>) )
import           Lib                            ( Flags(Flags)
                                                , getResult
                                                , getWordLineCharCount
                                                )
import           Options.Applicative            ( (<$>)
                                                , (<**>)
                                                , Applicative(pure)
                                                , Parser
                                                , argument
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , idm
                                                , info
                                                , progDesc
                                                , short
                                                , str
                                                , switch
                                                )
import           System.IO                      ( Handle
                                                , IOMode(ReadMode)
                                                , hFileSize
                                                , hGetLine
                                                , withFile
                                                )

data CommandOptions = CommandOptions
  { c           :: Bool
  , l           :: Bool
  , m           :: Bool
  , w           :: Bool
  , optFileName :: String
  }

optsParser :: Parser CommandOptions
optsParser = do
  c <- switch
    (  short 'c'
    <> help
         "The number of bytes in each input file is written to the standard output.  This will cancel out any prior usage of the -m option."
    )
  l <- switch
    (  short 'l'
    <> help
         "The number of lines in each input file is written to the standard output."
    )
  m <- switch
    (  short 'm'
    <> help
         "The number of characters in each input file is written to the standard output.  If the current locale does not support multibyte characters, this is equivalent to the -c option.  This will cancel out any prior usage of the -c option."
    )
  w <- switch
    (  short 'w'
    <> help
         "The number of words in each input file is written to the standard output."
    )
  optFileName <- argument str idm
  pure CommandOptions { .. }


main :: IO ()
main = do
  o <- execParser opts
  let fileName = optFileName o
  (wordCount, lineCount, charCount, size) <- withFile fileName
                                                      ReadMode
                                                      getFileDimensions
  putStrLn
    (getResult (Flags (c o) (l o) (m o) (w o))
               wordCount
               lineCount
               charCount
               size
               fileName
    )
 where
  opts = info
    (optsParser <**> helper)
    (  fullDesc
    <> progDesc
         "The wc utility displays the number of lines, words, and bytes contained in each input file, or standard input (if no file is specified) to the standard output.  A line is defined as a string of characters delimited by a ⟨newline⟩ character.  Characters beyond the final ⟨newline⟩ character will not be included in the line count."
    <> header "word-enumerator – word, line, character, and byte count"
    )

getFileDimensions :: Handle -> IO (Integer, Integer, Integer, Integer)
getFileDimensions handle = do
  size                              <- hFileSize handle
  (wordCount, lineCount, charCount) <- getWordLineCharCount handle
  return (wordCount, lineCount, charCount, size)
