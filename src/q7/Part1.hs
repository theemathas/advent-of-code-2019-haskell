-- Copied from q5/Part2.hs then edited

import Control.Exception (assert)
import Data.List (maximum)
import Data.Sequence (update, (!?))
import MyPrelude
import Relude.Unsafe (fromJust)
import Text.Parsec (char, eof, newline, sepBy)

main :: IO ()
main = do
  program <- parseFromFileOrError parseInput "src/q7/input.txt"
  print $ maximum $ map (evaluatePhaseSequence program) $ permutations [0 .. 4]

evaluatePhaseSequence :: Seq Integer -> [Integer] -> Integer
evaluatePhaseSequence program = foldl' oneAmplifier 0
  where
    oneAmplifier :: Integer -> Integer -> Integer
    oneAmplifier inputValue phase =
      let outputs = fromJust $ runIntcodeProgram program [phase, inputValue]
       in assert (length outputs == 1) $ head $ fromJust $ nonEmpty outputs

parseInput :: Parser (Seq Integer)
parseInput = do
  numbers <- decimal `sepBy` char ','
  newline >> eof
  return $ fromList numbers

--------------------------------------------------
-- Set up the interpreter types and utility functions

data InterpreterState = InterpreterState
  { memory :: Seq Integer,
    programCounter :: Integer,
    remainingInputs :: [Integer]
  }

type Interpreter = MaybeT (State InterpreterState)

runInterpreter :: Interpreter a -> InterpreterState -> (Maybe a, InterpreterState)
runInterpreter = runState . runMaybeT

getMemoryAt :: Integer -> Interpreter Integer
getMemoryAt address = do
  mem <- gets memory
  hoistMaybe $ mem !? fromInteger address

setMemoryAt :: Integer -> Integer -> Interpreter ()
setMemoryAt address value = do
  mem <- gets memory
  guard (0 <= address && fromInteger address < length mem)
  let newMem = update (fromInteger address) value mem
  modify $ \interpreterState -> interpreterState {memory = newMem}

getProgramCounter :: Interpreter Integer
getProgramCounter = programCounter <$> get

setProgramCounter :: Integer -> Interpreter ()
setProgramCounter value = modify $ \interpreterState -> interpreterState {programCounter = value}

advanceProgramCounter :: Integer -> Interpreter ()
advanceProgramCounter value = setProgramCounter =<< ((value +) <$> getProgramCounter)

getNextInput :: Interpreter Integer
getNextInput = do
  interpreterState <- get
  nextInput :| newRemainingInputs <- hoistMaybe (nonEmpty (remainingInputs interpreterState))
  put interpreterState {remainingInputs = newRemainingInputs}
  return nextInput

--------------------------------------------------
-- Actually implement the interpreter

-- Returns the outputs (or None if the program is faulty)
runIntcodeProgram :: Seq Integer -> [Integer] -> Maybe [Integer]
runIntcodeProgram initialMemory inputs = result
  where
    initialState = InterpreterState initialMemory 0 inputs
    (result, _finalState) = runInterpreter stepUntilHalt initialState

-- Returns the outputs
stepUntilHalt :: Interpreter [Integer]
stepUntilHalt = do
  pc <- getProgramCounter
  instruction <- getMemoryAt pc
  if instruction == 99
    then return [] -- Halt
    else do
      stepResult <- step
      restResult <- stepUntilHalt
      return $ stepResult ++ restResult

-- Returns the outputs
step :: Interpreter [Integer]
step = do
  pc <- getProgramCounter
  instruction <- getMemoryAt pc
  -- The function should never be called when the current instruction is halt (99)
  let opcode = assert (instruction /= 99) (instruction `mod` 100)
  let paramMode1 = instruction `div` 100 `mod` 10
  let paramMode2 = instruction `div` 1000 `mod` 10
  let paramMode3 = instruction `div` 10000

  -- Helper function:
  -- The behavior of an opcode that does something on the first two parameters
  -- and stores the result in the third parameter
  let opcodeCompute = \op -> do
        x <- readParameterAt (pc + 1) paramMode1
        y <- readParameterAt (pc + 2) paramMode2
        writeParameterAt (pc + 3) paramMode3 (op x y)
        advanceProgramCounter 4
        return []

  case opcode of
    -- "plus" opcode
    1 -> opcodeCompute (+)
    -- "multiply" opcode
    2 -> opcodeCompute (*)
    -- "input" opcode
    3 -> do
      guard (paramMode2 == 0 && paramMode3 == 0)
      nextInput <- getNextInput
      writeParameterAt (pc + 1) paramMode1 nextInput
      advanceProgramCounter 2
      return []
    -- "output" opcode
    4 -> do
      guard (paramMode2 == 0 && paramMode3 == 0)
      value <- readParameterAt (pc + 1) paramMode1
      advanceProgramCounter 2
      return [value]
    -- "jump-if-true" opcode
    5 -> do
      guard (paramMode3 == 0)
      x <- readParameterAt (pc + 1) paramMode1
      y <- readParameterAt (pc + 2) paramMode2
      if x /= 0
        then setProgramCounter y
        else advanceProgramCounter 3
      return []
    -- "jump-if-false" opcode
    6 -> do
      guard (paramMode3 == 0)
      x <- readParameterAt (pc + 1) paramMode1
      y <- readParameterAt (pc + 2) paramMode2
      if x == 0
        then setProgramCounter y
        else advanceProgramCounter 3
      return []
    -- "less-than" opcode
    7 -> opcodeCompute $ \x y -> if x < y then 1 else 0
    -- "equals" opcode
    8 -> opcodeCompute $ \x y -> if x == y then 1 else 0
    _ -> mzero

readParameterAt :: Integer -> Integer -> Interpreter Integer
readParameterAt paramAddress mode = do
  paramValue <- getMemoryAt paramAddress
  case mode of
    -- position mode
    0 -> getMemoryAt paramValue
    -- immediate mode
    1 -> return paramValue
    _ -> mzero

writeParameterAt :: Integer -> Integer -> Integer -> Interpreter ()
writeParameterAt paramAddress mode value = do
  guard (mode == 0) -- Only position mode supported
  paramValue <- getMemoryAt paramAddress
  setMemoryAt paramValue value