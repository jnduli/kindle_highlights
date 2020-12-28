module ParserSpec
  ( spec
  )
where

import           Test.Hspec
import           KindleHighlights
import           Text.Parsec

sampleHighlight1 :: String
sampleHighlight1 =
  "book1\n\
   \- Your Highlight on page 100-102 | Added on Wednesday, 24 October 2020 04:00:00\n\
   \\n\
   \book1 content\n\
   \==========\r\n"

sampleHighlight2 :: String
sampleHighlight2 =
  "book2\n\
   \- Your Highlight at location 37-37 | Added on Sunday, 28 October 2018 08:42:11\n\
   \\n\
   \book2 content\n\
   \==========\r\n"

sampleHighlights :: String
sampleHighlights = sampleHighlight1 ++ sampleHighlight2

spec :: Spec
spec = describe "KindleHighlights" $ do
  it "has a string definition for end of group"
    $          eogString
    `shouldBe` "==========\r\n"

  it "highlights" $ parse highlight "fail" sampleHighlight1 `shouldBe` Right
    (KindleHighlight "book1" "Highlight" 100 102 "book1 content")

  it "highlights 2" $ parse highlight "fail" sampleHighlight2 `shouldBe` Right
    (KindleHighlight "book2" "Highlight" 37 37 "book2 content")

  it "groups" $ parse groups "fail" sampleHighlights `shouldBe` Right
    [ KindleHighlight "book1" "Highlight" 100 102 "book1 content"
    , KindleHighlight "book2" "Highlight" 37  37  "book2 content"
    ]
