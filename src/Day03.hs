{-# LANGUAGE OverloadedStrings #-}

module Day03
  ( day3asol,
    parseAsMult,
    MultiplyInstruction (..),
    parseAsInstructions,
    carryOutInstructions,
    parseAndEval,
    day3aex,

  )
where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

-- Types
data MultiplyInstruction
  = Mult Int Int
  | Invalid
  deriving (Show, Eq)

type Instructions = [MultiplyInstruction]

-- parsing Input
parseMult :: Parser MultiplyInstruction
parseMult = do
  _ <- string "mul("
  num1 <- many1 digit <* char ','
  num2 <- many1 digit
  _ <- string ")"
  return $ Mult (read num1) (read num2)

parseInvalid :: Parser MultiplyInstruction
parseInvalid = Invalid <$ anyChar

multiplyInstructionParser = try parseMult <|> parseInvalid

instructionsparser :: Parser Instructions
instructionsparser = many multiplyInstructionParser

parseAsMult :: T.Text -> Either ParseError MultiplyInstruction
parseAsMult = parse multiplyInstructionParser "no file"

parseAsInstructions :: T.Text -> Either ParseError Instructions
parseAsInstructions = parse instructionsparser "no file"

-- Part 1

-- | Evaluates MultiPlyInstruction, returning 0 on Invlid
multiplyValid :: MultiplyInstruction -> Int
multiplyValid Invalid = 0
multiplyValid (Mult a b) = a * b

carryOutInstructions :: [MultiplyInstruction] -> Int
carryOutInstructions = sum . map multiplyValid

parseAndEval :: T.Text -> Either ParseError Int
parseAndEval t = carryOutInstructions <$> parseAsInstructions t

day3a :: String -> IO ()
day3a fname = do
  contents <- readFile fname
  print $ parseAndEval $ T.pack contents

day3asol :: IO ()
day3asol = day3a "input/day03.txt"

day3aex :: IO ()
day3aex= day3a "input/day03example.txt"