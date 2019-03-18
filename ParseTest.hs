module ParseTest where

import Parse
import Test.HUnit

testEmptyInput :: Test
testEmptyInput =
    TestCase $ assertEqual "Should return empty list"
        [] (parseDoc [])

testTrivialHeader :: Test
testTrivialHeader =
    TestCase $ assertEqual "Should return Heading block"
        [(Heading 1 "Header")] (parseDoc "# Header")

testParseCorrectHeadingSize =
    TestCase $ assertEqual "Should return level 3 Heading block"
        [(Heading 3 "Header")] (parseDoc "### Header")

testParseMultipleHeadings =
    TestCase $ assertEqual "Should return Headings 1-6"
        [(Heading 1 "One"),
        (Heading 2 "Two"),
        (Heading 3 "Three"),
        (Heading 4 "Four"),
        (Heading 5 "Five"),
        (Heading 6 "Six")]
        (parseDoc "# One\n## Two\n### Three\n#### Four\n##### Five\n###### Six")

testTrivialBoldText =
    TestCase $ assertEqual "Should return a Bold inline"
        [(Para [(Bold (Plain ("bold")))])] (parseDoc "**bold**")

testTrivialItalicText =
    TestCase $ assertEqual "Should return an Italic inline"
        [(Para [(Italic (Plain ("italic")))])] (parseDoc "*italic*")

testItalicAndBold =
    TestCase $ assertEqual "Should return a Bold Italic inline"
        [(Para [Bold (Italic (Plain ("italic bold")))])] (parseDoc "***italic bold***")

testTrivialLinkText =
    TestCase $ assertEqual "Should return a Link inline"
        [(Para [(Link "display text" "https://www.google.com/")])] (parseDoc "[display text](https://www.google.com/)")

testTrivialCodeText =
    TestCase $ assertEqual "Should return a Code inline"
        [(Para [(Code "import Markerel")])] (parseDoc "`import Markerel`")

testTrivialPlainText =
    TestCase $ assertEqual "Should return a Plain inline"
        [(Para [(Plain "Hello, world!")])] (parseDoc "Hello, world!")

testDocumentIsDividedIntoParagraphs =
    TestCase $ assertEqual "Should return 3 paragraphs"
        3 (length (parseDoc "## I'm a header!\nI'm paragraph number #2!\nI'm *paragraph* number 3"))

testBlankParagraphsAreRemoved =
    TestCase $ assertEqual "Should return 3 paragraphs"
        3 (length (parseDoc "## I'm a header!\n\n\n\n\n\nI'm paragraph number #2!\n\n\n\n\n\nI'm *paragraph* number 3"))

main :: IO Counts
main = runTestTT $ TestList [
        testEmptyInput,
        testTrivialHeader,
        testParseCorrectHeadingSize,
        testParseMultipleHeadings,
        testTrivialBoldText,
        testTrivialItalicText,
        testItalicAndBold,
        testTrivialLinkText,
        testTrivialCodeText,
        testTrivialPlainText,
        testDocumentIsDividedIntoParagraphs,
        testBlankParagraphsAreRemoved]
