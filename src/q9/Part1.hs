-- Copied from q7/Part2.hs then edited

import Control.Exception (assert)
import Data.Sequence (update, (!?), (><))
import qualified Data.Sequence as S (replicate)
import MyPrelude
import Pipes (Pipe (..), await, evalStateP, feed, getOutputs, yield)
import Text.Parsec (char, eof, newline, sepBy)

main :: IO ()
main = do
  program <- parseFromFileOrError parseInput "src/q9/input.txt"
  print $ join $ getOutputs $ feed 1 $ runIntcodeProgram program

parseInput :: Parser (Seq Integer)
parseInput = do
  numbers <- decimal `sepBy` char ','
  newline >> eof
  return $ fromList numbers

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
    -- "adjust relative base" opcode
    9 -> do
      guard (paramMode2 == 0 && paramMode3 == 0)
      value <- readParameterAt (pc + 1) paramMode1
      adjustRelativeBase value
      advanceProgramCounter 2
    _ -> mzero

readParameterAt :: Integer -> Integer -> Interpreter Integer
readParameterAt paramAddress mode = do
  paramValue <- getMemoryAt paramAddress
  case mode of
    -- position mode
    0 -> getMemoryAt paramValue
    -- immediate mode
    1 -> return paramValue
    -- relative mode
    2 -> do
      base <- getRelativeBase
      getMemoryAt (base + paramValue)
    _ -> mzero

writeParameterAt :: Integer -> Integer -> Integer -> Interpreter ()
writeParameterAt paramAddress mode value = do
  paramValue <- getMemoryAt paramAddress
  case mode of
    -- position mode
    0 -> setMemoryAt paramValue value
    -- relative mode
    2 -> do
      base <- getRelativeBase
      setMemoryAt (base + paramValue) value
    _ -> mzero

--------------------------------------------------
-- Interpreter types and utility functions

data InterpreterState = InterpreterState
  { memory :: Seq Integer,
    programCounter :: Integer,
    relativeBase :: Integer
  }

-- A "Nothing" in this represents an error due to an invalid program.
-- The "Pipe" part handles the inputs and outputs.
type Interpreter = Pipe Integer Integer (StateT InterpreterState Maybe)

initializeInterpreter :: Seq Integer -> Interpreter a -> Pipe Integer Integer Maybe a
initializeInterpreter initialMemory = evalStateP (InterpreterState initialMemory 0 0)

-- Ensures that the address given is non-negative. And if the current memory
-- isn't long enough, extend it with zeroes so we can access that address.
ensureMemoryAccessibleAt :: Integer -> Interpreter ()
ensureMemoryAccessibleAt address = do
  guard (address >= 0)
  mem <- gets memory
  let memLength = toInteger (length mem)
  when (address >= memLength) $
    let newMemory = mem >< S.replicate (fromInteger (address + 1 - memLength)) 0
     in assert (address < toInteger (length newMemory)) $
          modify $ \interpreterState -> interpreterState {memory = newMemory}

getMemoryAt :: Integer -> Interpreter Integer
getMemoryAt address = do
  ensureMemoryAccessibleAt address
  mem <- gets memory
  lift $ lift $ mem !? fromInteger address

setMemoryAt :: Integer -> Integer -> Interpreter ()
setMemoryAt address value = do
  ensureMemoryAccessibleAt address
  mem <- gets memory
  let newMem = update (fromInteger address) value mem
  modify $ \interpreterState -> interpreterState {memory = newMem}

getProgramCounter :: Interpreter Integer
getProgramCounter = programCounter <$> get

setProgramCounter :: Integer -> Interpreter ()
setProgramCounter value = modify $ \interpreterState -> interpreterState {programCounter = value}

advanceProgramCounter :: Integer -> Interpreter ()
advanceProgramCounter value = (setProgramCounter . (value +)) =<< getProgramCounter

getRelativeBase :: Interpreter Integer
getRelativeBase = relativeBase <$> get

setRelativeBase :: Integer -> Interpreter ()
setRelativeBase value = modify $ \interpreterState -> interpreterState {relativeBase = value}

adjustRelativeBase :: Integer -> Interpreter ()
adjustRelativeBase value = (setRelativeBase . (value +)) =<< getRelativeBase