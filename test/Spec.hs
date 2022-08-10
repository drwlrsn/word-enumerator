import           Lib
import           System.IO                      ( IOMode(ReadMode)
                                                , openFile
                                                )
import           Test.Hspec

-- wordCount lineCount charCount size fileName
wordCount = 1 :: Integer

lineCount = 2 :: Integer

charCount = 3 :: Integer

size = 4 :: Integer

fileName = "test.txt"

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    describe "getResult" $ do
      it "should handle -c" $ do
        getResult
            Flags { optc = True, optl = False, optm = False, optw = False }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat [tab, show size, " ", fileName]
      it "should handle -l" $ do
        getResult
            Flags { optc = False, optl = True, optm = False, optw = False }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat [tab, show lineCount, " ", fileName]
      it "should handle -m" $ do
        getResult
            Flags { optc = False, optl = False, optm = True, optw = False }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat [tab, show charCount, " ", fileName]
      it "should handle -w" $ do
        getResult
            Flags { optc = False, optl = False, optm = False, optw = True }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat [tab, show wordCount, " ", fileName]
      it "should handle -cl" $ do
        getResult
            Flags { optc = True, optl = True, optm = False, optw = False }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat [tab, show lineCount, tab, show size, " ", fileName]
      it "should handle -cm" $ do
        getResult
            Flags { optc = True, optl = False, optm = True, optw = False }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat [tab, show size, " ", fileName]
      it "should handle -cw" $ do
        getResult
            Flags { optc = True, optl = False, optm = False, optw = True }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat [tab, show wordCount, tab, show size, " ", fileName]
      it "should handle -clm" $ do
        getResult
            Flags { optc = True, optl = True, optm = True, optw = False }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat [tab, show lineCount, tab, show size, " ", fileName]
      it "should handle -lm" $ do
        getResult
            Flags { optc = False, optl = True, optm = True, optw = False }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat
                       [tab, show lineCount, tab, show charCount, " ", fileName]
      it "should handle -lw" $ do
        getResult
            Flags { optc = False, optl = True, optm = False, optw = True }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat
                       [tab, show lineCount, tab, show wordCount, " ", fileName]
      it "should handle -mw" $ do
        getResult
            Flags { optc = False, optl = False, optm = True, optw = True }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat
                       [tab, show wordCount, tab, show charCount, " ", fileName]
      it "should handle -lmw" $ do
        getResult
            Flags { optc = False, optl = True, optm = True, optw = True }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat
                       [ tab
                       , show lineCount
                       , tab
                       , show wordCount
                       , tab
                       , show charCount
                       , " "
                       , fileName
                       ]
      it "should handle no flags" $ do
        getResult
            Flags { optc = False, optl = False, optm = False, optw = False }
            wordCount
            lineCount
            charCount
            size
            fileName
          `shouldBe` concat
                       [ tab
                       , show lineCount
                       , tab
                       , show wordCount
                       , tab
                       , show size
                       , " "
                       , fileName
                       ]

    describe "countFileWords" $ do
      it "should handle non-english" $ do
        handle <- openFile "test/data/test1.txt" ReadMode
        actual <- countFileWords handle
        actual `shouldBe` (3, 1, 11)
      it "should handle non-english with empty last line" $ do
        handle <- openFile "test/data/test2.txt" ReadMode
        actual <- countFileWords handle
        actual `shouldBe` (3, 0, 10)

-- it "should handle -l" $ do
-- getResult (Flags False True False False) wordCount lineCount charCount size fileName `shouldBe` tab ++ show lineCount ++ " " ++ fileName
