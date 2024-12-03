{-# LANGUAGE OverloadedStrings #-}

module Day03
  ( day3asol,
    day3bsol,
    parseAsMult,
    MultiplyInstruction (..),
    parseAsInstructions,
    carryOutInstructions,
    parseAndEval,
    day3aex,
    filterEnabled,
    parseFilterAndEval,
  )
where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

-- Types
data MultiplyInstruction
  = Mult Int Int
  | Invalid
  | Do
  | Dont
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

parseDo :: Parser MultiplyInstruction
parseDo = Do <$ string "do"

parseDont :: Parser MultiplyInstruction
parseDont = Dont <$ string "don't"

multiplyInstructionParser :: Parser MultiplyInstruction
multiplyInstructionParser = try parseMult <|> try parseDont <|> try parseDo <|> parseInvalid

instructionsparser :: Parser Instructions
instructionsparser = many multiplyInstructionParser

parseAsMult :: T.Text -> Either ParseError MultiplyInstruction
parseAsMult = parse multiplyInstructionParser "no file"

parseAsInstructions :: T.Text -> Either ParseError Instructions
parseAsInstructions = parse instructionsparser "no file"

-- Part 1

-- | Evaluates MultiPlyInstruction, returning 0 on Invalid
multiplyValid :: MultiplyInstruction -> Int
multiplyValid Invalid = 0
multiplyValid Do = 0
multiplyValid Dont = 0
multiplyValid (Mult a b) = a * b

-- | carries out all Intructions
carryOutInstructions :: [MultiplyInstruction] -> Int
carryOutInstructions = sum . map multiplyValid

-- | parse the Instructions, then carry them out
parseAndEval :: T.Text -> Either ParseError Int
parseAndEval t = carryOutInstructions <$> parseAsInstructions t

-- | read input file specified by fname, then solve and print result
day3a :: String -> IO ()
day3a fname = do
  contents <- readFile fname
  print $ parseAndEval $ T.pack contents

-- | solution for actual input
day3asol :: IO ()
day3asol = day3a "input/day03.txt"

-- | solution for example input
day3aex :: IO ()
day3aex = day3a "input/day03example.txt"

-- Part 2
filterEnabled :: Instructions -> Instructions
filterEnabled [] = []
filterEnabled instructionlist =
  takeWhile (/= Dont) instructionlist
    ++ filterEnabled
      ( dropWhile (== Do) $
          dropWhile (/= Do) $
            dropWhile (/= Dont) instructionlist
      )

-- | parse the Instructions, then carry them out
parseFilterAndEval :: T.Text -> Either ParseError Int
parseFilterAndEval t = carryOutInstructions . filterEnabled <$> parseAsInstructions t

day3b :: String -> IO ()
day3b fname = do
  contents <- readFile fname
  print $ parseFilterAndEval $ T.pack contents

-- | solution for actual input
day3bsol :: IO ()
day3bsol = day3b "input/day03.txt"