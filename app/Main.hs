{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where

import Lib
import System.IO (openFile, IOMode (ReadMode), hGetLine, Handle, hClose, hFileSize)
import Data.Text (split, pack, unpack, Text, null, empty)
import GHC.TypeLits (ErrorMessage(Text))
import Prelude hiding (null)
import System.IO.Error (catchIOError, isEOFError)

splitOnSpace :: Text -> [Text]
splitOnSpace = split (\c -> c == ' ' || c == '\t')

countLineWords = length . splitOnSpace

countFileWords :: Int -> Handle -> IO Int
countFileWords count handle =  do
   line <- pack <$> catchIOError (hGetLine handle) (\e -> if isEOFError e then return "" else ioError e)
   if null line then pure count else  countFileWords (count + countLineWords line) handle



main :: IO ()
main =  do
  handle <- openFile "test.txt" ReadMode
  count <- countFileWords 0 handle
  size <- hFileSize handle
  _ <- hClose handle
  putStrLn $ "\t" ++ show count ++ "\t" ++ show size
