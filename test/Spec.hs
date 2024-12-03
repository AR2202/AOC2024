{-# LANGUAGE OverloadedStrings #-}
import Day03
import Test.QuickCheck
import Test.Hspec
import qualified Data.Text as T
import Day03 (parseAsInstructions, carryOutInstructions, parseAndEval)
main :: IO ()
main =  hspec $
  do
    -- Parsing
    -----------------------------------------
    testDay03
    testMultParserValid
    testMultParserInvalid
    testInstructionExample

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
    `shouldBe` Right (Mult 11 8 )

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
instParserExample = take 12 <$>
  parseAsInstructions "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    `shouldBe` Right [Invalid, 
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
    Invalid]

testInstructionParserExample :: SpecWith ()
testInstructionParserExample =
  describe "parseAsInstructions" $
    context "when parsing the example input" $
      it
        "should return a list of MultiplyInstruction"
        instParserExample

evalExample :: Expectation
evalExample  = 
  carryOutInstructions [Invalid, Mult 2 4, Invalid, Mult 5 5 , Invalid, Mult 11 8,Mult 8 5]
    `shouldBe` 161
testInstructionExample :: SpecWith ()
testInstructionExample =
  describe "carryOutInstructions" $
    context "when evaluating the example input" $
      it
        "should return 161"
        evalExample