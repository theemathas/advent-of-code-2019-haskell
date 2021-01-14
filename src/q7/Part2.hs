-- Now with pipes!

import Control.Exception (assert)
import Data.List (foldl1', maximum)
import Data.Sequence (update, (!?), (|>))
import qualified Data.Sequence as S
import MyPrelude
import Pipes (Pipe (..), await, evalStateP, feed, yield, (>->))
import Relude.Unsafe (fromJust)
import Text.Parsec (char, eof, newline, sepBy)

main :: IO ()
main = do
  program <- parseFromFileOrError parseInput "src/q7/input.txt"
  print $ maximum $ mapMaybe (evaluatePhaseSequence program) $ permutations [5 .. 9]

parseInput :: Parser (Seq Integer)
parseInput = do
  numbers <- decimal `sepBy` char ','
  newline >> eof
  return $ fromList numbers

evaluatePhaseSequence :: Seq Integer -> [Integer] -> Maybe Integer
evaluatePhaseSequence program phases = go empty combinedAmplifier
  where
    amplifiers :: [Pipe Integer Integer Maybe ()]
    amplifiers = map (\phase -> feed phase (runIntcodeProgram program)) phases
    -- Linearly combined, but not looped around
    combinedAmplifier :: Pipe Integer Integer Maybe ()
    combinedAmplifier = feed 0 (foldl1' (>->) amplifiers)

    -- Helper function to loop around the amplifiers.
    -- For the queue: head is front and last is back of queue.
    -- It's a queue of outputs that have been outputted by the pipe but not yet
    -- consumed by it.
    go :: Seq Integer -> Pipe Integer Integer Maybe () -> Maybe Integer
    go queue (PipeIn inFunc) = do
      firstInput <- Just (fromJust (queue !? 0)) -- TODO this crashed
      let remainingQueue = S.drop 1 queue
      go remainingQueue (inFunc firstInput)
    go queue (PipeOut outValue pipe) = go (queue |> outValue) pipe
    go queue (PipeM m) = go queue =<< m
    go queue (PipePure ()) = Just (fromJust (queue !? 0))

--------------------------------------------------
-- Main intcode implementation

runIntcodeProgram :: Seq Integer -> Pipe Integer Integer Maybe ()
runIntcodeProgram initialMemory = initializeInterpreter initialMemory stepUntilHalt

-- Returns the outputs
stepUntilHalt :: Interpreter ()
stepUntilHalt = do
  pc <- getProgramCounter
  instruction <- getMemoryAt pc
  if instruction == 99
    then return () -- Halt
    else step >> stepUntilHalt

-- Returns the outputs
step :: Interpreter ()
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

  case opcode of
    -- "plus" opcode
    1 -> opcodeCompute (+)
    -- "multiply" opcode
    2 -> opcodeCompute (*)
    -- "input" opcode
    3 -> do
      guard (paramMode2 == 0 && paramMode3 == 0)
      nextInput <- await
      writeParameterAt (pc + 1) paramMode1 nextInput
      advanceProgramCounter 2
    -- "output" opcode
    4 -> do
      guard (paramMode2 == 0 && paramMode3 == 0)
      value <- readParameterAt (pc + 1) paramMode1
      advanceProgramCounter 2
      yield value
    -- "jump-if-true" opcode
    5 -> do
      guard (paramMode3 == 0)
      x <- readParameterAt (pc + 1) paramMode1
      y <- readParameterAt (pc + 2) paramMode2
      if x /= 0
        then setProgramCounter y
        else advanceProgramCounter 3
    -- "jump-if-false" opcode
    6 -> do
      guard (paramMode3 == 0)
      x <- readParameterAt (pc + 1) paramMode1
      y <- readParameterAt (pc + 2) paramMode2
      if x == 0
        then setProgramCounter y
        else advanceProgramCounter 3
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

--------------------------------------------------
-- Interpreter types and utility functions

data InterpreterState = InterpreterState {memory :: Seq Integer, programCounter :: Integer}

-- A "Nothing" in this represents an error due to an invalid program.
-- The "Pipe" part handles the inputs and outputs.
type Interpreter = Pipe Integer Integer (StateT InterpreterState Maybe)

initializeInterpreter :: Seq Integer -> Interpreter a -> Pipe Integer Integer Maybe a
initializeInterpreter initialMemory = evalStateP (InterpreterState initialMemory 0)

getMemoryAt :: Integer -> Interpreter Integer
getMemoryAt address = do
  mem <- gets memory
  lift $ lift $ mem !? fromInteger address

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
advanceProgramCounter value = (setProgramCounter . (value +)) =<< getProgramCounter