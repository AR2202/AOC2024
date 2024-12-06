{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Day03
import Day03 (carryOutInstructions, parseAndEval, parseAsInstructions, parseFilterAndEval)
import Day04
import Day04 (countXmasDiagRevLines, 
  countXmasDiagRevUpLines, 
  countXmasDiagUpLines, 
  countXmasReverse, 
  countXmasReverseTranspose)
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Error

main :: IO ()
main = hspec $
  do
    -- Day03
    -----------------------------------------
    testDay03
    testMultParserValid
    testMultParserInvalid
    testInstructionExample
    testfilterEnabledWorks
    testevalDay3Part2multipleDisablesEnables
    testfilterEnabledMultiple
    testfilterEnabledMultipleDo

    --Day04
    ---------------------------
    testInfix
    testCountReverse
    testCountReverseT
    testRotate
    testcountDiagUp
    testcountExample
    testCountT
    testcountDiagR

-- Day03
-----------------------------------------
testDay03MatchExample :: Expectation
testDay03MatchExample =
  parseAndEval "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    `shouldBe` Right 161

testDay03 :: SpecWith ()
testDay03 =
  describe "Day03" $
    context "when evaluating example" $
      it
        "should return 161"
        testDay03MatchExample

multParserValid :: Expectation
multParserValid =
  parseAsMult "mul(11,8)"
    `shouldBe` Right (Mult 11 8)

testMultParserValid :: SpecWith ()
testMultParserValid =
  describe "parseAsMult" $
    context "when evaluating valid input" $
      it
        "should return he MultiplyInstruction"
        multParserValid

multParserInvalid :: Expectation
multParserInvalid =
  parseAsMult "mul(6,9!"
    `shouldBe` Right Invalid

testMultParserInvalid :: SpecWith ()
testMultParserInvalid =
  describe "parseAsMult" $
    context "when evaluating invalid input" $
      it
        "should return Invalid"
        multParserInvalid

instParserValid :: Expectation
instParserValid =
  parseAsInstructions "xmul(2,4)%"
    `shouldBe` Right [Invalid, Mult 2 4, Invalid]

testInstructionParser :: SpecWith ()
testInstructionParser =
  describe "parseAsInstructions" $
    context "when evaluating valid input" $
      it
        "should return a list of MultiplyInstruction"
        instParserValid

instParserExample :: Expectation
instParserExample =
  take 12
    <$> parseAsInstructions "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    `shouldBe` Right
      [ Invalid,
        Mult 2 4,
        Invalid,
        Invalid,
        Invalid,
        Invalid,
        Invalid,
        Invalid,
        Invalid,
        Invalid,
        Invalid,
        Invalid
      ]

testInstructionParserExample :: SpecWith ()
testInstructionParserExample =
  describe "parseAsInstructions" $
    context "when parsing the example input" $
      it
        "should return a list of MultiplyInstruction"
        instParserExample

evalExample :: Expectation
evalExample =
  carryOutInstructions [Invalid, Mult 2 4, Invalid, Mult 5 5, Invalid, Mult 11 8, Mult 8 5]
    `shouldBe` 161

testInstructionExample :: SpecWith ()
testInstructionExample =
  describe "carryOutInstructions" $
    context "when evaluating the example input" $
      it
        "should return 161"
        evalExample

filterEnabledWorks :: Expectation
filterEnabledWorks =
  filterEnabled [Invalid, Mult 2 4, Invalid, Mult 5 5, Dont, Mult 11 8, Do, Mult 8 5]
    `shouldBe` [Invalid, Mult 2 4, Invalid, Mult 5 5, Mult 8 5]

testfilterEnabledWorks :: SpecWith ()
testfilterEnabledWorks =
  describe "filterEnabled" $
    it
      "should return Instructions enabled with Do"
      filterEnabledWorks

filterEnabledMultiple :: Expectation
filterEnabledMultiple =
  filterEnabled [Invalid, Mult 2 4, Invalid, Mult 5 5, Dont, Mult 11 8, Do, Mult 8 5, Dont, Mult 11 8, Do, Mult 8 5, Mult 4 5]
    `shouldBe` [Invalid, Mult 2 4, Invalid, Mult 5 5, Mult 8 5, Mult 8 5, Mult 4 5]

testfilterEnabledMultiple :: SpecWith ()
testfilterEnabledMultiple =
  describe "filterEnabled" $
    context "when evaluating larger input" $
      it
        "should return Instructions enabled with Do"
        filterEnabledMultiple

filterEnabledMultipleDo :: Expectation
filterEnabledMultipleDo =
  filterEnabled [Invalid, Mult 2 4, Invalid, Mult 5 5, Dont, Mult 11 8, Do, Mult 8 5, Do, Mult 11 8, Do, Mult 8 5, Dont, Mult 4 5, Do, Mult 10 1]
    `shouldBe` [Invalid, Mult 2 4, Invalid, Mult 5 5, Mult 8 5, Do, Mult 11 8, Do, Mult 8 5, Mult 10 1]

testfilterEnabledMultipleDo :: SpecWith ()
testfilterEnabledMultipleDo =
  describe "filterEnabled" $
    context "when evaluating larger input" $
      it
        "should return Instructions enabled with Do"
        filterEnabledMultipleDo

evalDay3Part2Example :: Expectation
evalDay3Part2Example =
  parseFilterAndEval "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    `shouldBe` (Right 48 :: Either ParseError Int)

testevalDay3Part2Example :: SpecWith ()
testevalDay3Part2Example =
  describe "parseFiltereval" $
    context "when evaluating the example input" $
      it
        "should return Instructions enabled with Do"
        evalDay3Part2Example

evalDay3Part2multipleDisablesEnables :: Expectation
evalDay3Part2multipleDisablesEnables =
  parseFilterAndEval "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))don'tmul(3,4)543mul(9,10)do6790(mul(2,1))"
    `shouldBe` (Right 50 :: Either ParseError Int)

testevalDay3Part2multipleDisablesEnables :: SpecWith ()
testevalDay3Part2multipleDisablesEnables =
  describe "parseFiltereval" $
    context "when evaluating longer input" $
      it
        "should return Instructions enabled with Do"
        evalDay3Part2multipleDisablesEnables

-- Day04
----------------
xmasInfix :: Expectation
xmasInfix =
  findXmas "MMXMASDD"
    `shouldBe` True

testInfix :: SpecWith ()
testInfix =
  describe "findXmas" $
    context "when evaluating a string with xmas" $
      it
        "should return True"
        xmasInfix

countRev :: Expectation
countRev =
  countXmasReverse "MMXMASDD\nSAMXMASAMXMAS"
    `shouldBe` 2

testCountReverse :: SpecWith ()
testCountReverse =
  describe "countXmasReverse" $
    it
      "should count the backwards XMAS words in each line"
      countRev

countRevT :: Expectation
countRevT =
  countXmasReverseTranspose "SSSSSS\nXMASSS\nSAMXMAS\nMMXABCD\nXXXMMM"
    `shouldBe` 1

testCountReverseT :: SpecWith ()
testCountReverseT =
  describe "countXmasReverseTranspose" $
    it
      "should count the backwards XMAS words in each column"
      countRevT

countT :: Expectation
countT =
  countXmasReverseTranspose "SSSSSS\nXMASSS\nMAMXMAS\nAMXABCD\nSXXMMM"
    `shouldBe` 1

testCountT :: SpecWith ()
testCountT =
  describe "countXmasTranspose" $
    it
      "should count the  XMAS words in each column"
      countT

countDiagUp :: Expectation
countDiagUp =
  countXmasDiagRevUpLines "SSSSSS\nXAMSSAS\nSAMXMAS\nMMAXBCD\nXXXMMM"
    `shouldBe` 1

testcountDiagUp :: SpecWith ()
testcountDiagUp =
  describe "countXmasDiagUp" $
    it
      "should count the diagonal XMAS words upwards"
      countDiagUp

countDiagR :: Expectation
countDiagR =
  countXmasDiagRevLines "SSSSSS\nXAMSSS\nSAMXAMS\nMMAXBMD\nXXXMMX"
    `shouldBe` 1

testcountDiagR :: SpecWith ()
testcountDiagR =
  describe "countXmasDiagRev" $
    it
      "should count the diagonal XMAS words upwards"
      countDiagR

countExample :: Expectation
countExample =
  countAll "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
    `shouldBe` 18

testcountExample :: SpecWith ()
testcountExample =
  describe "countAll" $
    it
      "should count all XMAS"
      countExample

rotated :: Expectation
rotated =
  rotate1 "ABCD"
    `shouldBe` "BCDA"

testRotate :: SpecWith ()
testRotate =
  describe "countXmasReverseTranspose" $
    it
      "should count the backwards XMAS words in each column"
      rotated