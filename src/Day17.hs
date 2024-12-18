module Day17 (evalExample,a, evalActual, lastDigits, convert2decimal) where

import Data.Bits

-- might change this as not all Ints are valid
type Operand = Int

data Opcode
  = Adv Operand
  | Bxl Operand
  | Bst Operand
  | Jnz Operand
  | Bxc Operand
  | Out Operand
  | Bdv Operand
  | Cdv Operand
  | Err
  deriving (Show, Read, Eq)

data ProgramState = ProgramState
  { registerA :: Int,
    registerB :: Int,
    registerC :: Int,
    instructionPointer :: Int,
    output :: [Int]
  }
  deriving (Show, Read, Eq)

-- | parses 2 numbers as the Opcode and ist Operand
parseAsOpcode :: [Operand] -> Opcode
parseAsOpcode [0, x] = Adv x
parseAsOpcode [1, x] = Bxl x
parseAsOpcode [2, x] = Bst x
parseAsOpcode [3, x] = Jnz x
parseAsOpcode [4, x] = Bxc x
parseAsOpcode [5, x] = Out x
parseAsOpcode [6, x] = Bdv x
parseAsOpcode [7, x] = Cdv x
parseAsOpcode _ = Err -- should not occur, but just to be sure

combo progState 4 = registerA progState
combo progState 5 = registerB progState
combo progState 6 = registerC progState
combo progState x = x

evalOpcode progState (Adv x) =
  progState
    { registerA = (registerA progState) `div` (2 ^ (combo progState x)),
      instructionPointer = instructionPointer progState + 2
    }
evalOpcode progState (Bxl x) =
  progState
    { registerB = xor (registerB progState) x,
      instructionPointer = instructionPointer progState + 2
    }
evalOpcode progState (Bst x) =
  progState
    { registerB = (combo progState x) `mod` 8,
      instructionPointer = instructionPointer progState + 2
    }
evalOpcode progState (Jnz x) =
  if registerA progState == 0
    then
      progState
        { instructionPointer = instructionPointer progState + 2
        }
    else
      progState
        { instructionPointer = x
        }
evalOpcode progState (Bxc _) =
  progState
    { registerB = xor (registerB progState) (registerC progState),
      instructionPointer = instructionPointer progState + 2
    }
evalOpcode progState (Out x) =
  progState
    { output = (combo progState x) `mod` 8 : output progState,
      instructionPointer = instructionPointer progState + 2
    }
evalOpcode progState (Bdv x) =
  progState
    { registerB = (registerA progState) `div` (2 ^ (combo progState x)),
      instructionPointer = instructionPointer progState + 2
    }
evalOpcode progState (Cdv x) =
  progState
    { registerC = (registerA progState) `div` (2 ^ (combo progState x)),
      instructionPointer = instructionPointer progState + 2
    }

runProgram progState program = case (take 2 . drop (instructionPointer progState)) program of
  [] -> reverse (output progState)
  ls -> runProgram (evalOpcode progState (parseAsOpcode ls)) program

example = ProgramState 729 0 0 0 []

exampleProg = [0, 1, 5, 4, 3, 0]

evalExample = runProgram example exampleProg

actual = ProgramState 63687530 0 0 0 []

actualProgram = [2, 4, 1, 3, 7, 5, 0, 3, 1, 5, 4, 1, 5, 5, 3, 0]

evalActual = runProgram actual actualProgram

-- Round1: at end of program, A needs to be non-zero, B mod 8 needs to be 2
-- one step before, B  xor C needs to end with 010
-- (B xor 5 ) xor C needs to end with 010, 5 is 101
-- the previous step divides A by 8
-- the previous register sets C to A divided by 32
-- the previous step does B xor 3, 3 is 11
-- the previous step sets B to A mod 8

-- B xor 110 = C xor head list
-- B = last 3 digits of A
-- C = digits -8 - -5 of A
-- the last digit of C becomes the first digit of B in the next round

xored = map (xor 6) actualProgram

lshifted = map (flip shift 2) xored

nums = zipWith xor (tail xored ++[0]) lshifted

lastDigits = map ((`mod` 2) . (`div` 4)) nums

convert2decimal [] _ = 0
convert2decimal (x:xs) n = x*(2^n) + convert2decimal xs (n+1)

digitsA = 0:0:0:1:0:0:lastDigits

a = convert2decimal digitsA 0