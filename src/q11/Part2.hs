import Control.Exception (assert)
import Data.Foldable (Foldable (maximum, minimum))
import Data.Sequence (update, (!?), (><))
import qualified Data.Sequence as Seq (replicate)
import qualified Data.Set as Set (delete, empty, insert, member)
import qualified Data.Text as Text (append, concat)
import MyPrelude
import Pipes (Pipe (..), await, evalStateP, yield)
import Text.Parsec (char, eof, newline, sepBy)

type Coord = (Integer, Integer) -- (x, y)

data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Show)

main :: IO ()
main = do
  program <- parseFromFileOrError parseInput "src/q11/input.txt"
  putText $ showPanels $ whitePanels $ runRobot program

parseInput :: Parser (Seq Integer)
parseInput = do
  numbers <- decimal `sepBy` char ','
  newline >> eof
  return $ fromList numbers

showPanels :: Set Coord -> Text
showPanels panels = Text.concat $ (`Text.append` "\n") <$> allRows
  where
    leftX = minimum $ map fst $ toList panels
    rightX = maximum $ map fst $ toList panels
    bottomY = minimum $ map snd $ toList panels
    topY = maximum $ map snd $ toList panels
    isWhite rowId colId = (leftX + colId, topY - rowId) `Set.member` panels

    panelAt rowId colId = if isWhite rowId colId then '█' {- U+2588X -} else ' '
    rowAt rowId = toText $ panelAt rowId <$> [0 .. (rightX - leftX)]
    allRows = rowAt <$> [0 .. (topY - bottomY)]

data RobotState = RobotState
  { robotProcessor :: Pipe Integer Integer Maybe (),
    robotDirection :: Direction,
    robotPosition :: Coord,
    whitePanels :: Set Coord,
    touchedPanels :: Set Coord
  }

-- Returns the final state
runRobot :: Seq Integer -> RobotState
runRobot program = go $ RobotState (runIntcodeProgram program) DirUp (0, 0) (one (0, 0)) Set.empty
  where
    -- Run until termination, and return the final state
    go :: RobotState -> RobotState
    go startState = case robotProcessor startState of
      PipeIn inFunc ->
        let pipe = inFunc $ colorAt (robotPosition startState) startState
         in go (startState {robotProcessor = pipe})
      PipeOut newColor pipe ->
        goTurn $ setColorAt (robotPosition startState) newColor $ startState {robotProcessor = pipe}
      PipeM Nothing -> error "Robot crashed"
      PipeM (Just pipe) -> go (startState {robotProcessor = pipe})
      PipePure () -> startState -- Gracefully terminated

    -- Used for the state after painting but before turning.
    goTurn :: RobotState -> RobotState
    goTurn startState =
      case robotProcessor startState of
        PipeIn inFunc ->
          let pipe = inFunc $ colorAt (robotPosition startState) startState
           in goTurn (startState {robotProcessor = pipe})
        PipeOut turnDirection pipe ->
          go $ moveRobotForward $ turnRobot turnDirection $ startState {robotProcessor = pipe}
        PipeM Nothing -> error "Robot crashed"
        PipeM (Just pipe) -> goTurn (startState {robotProcessor = pipe})
        PipePure () -> error "Robot terminated when it should output a direction"
    colorAt :: Coord -> RobotState -> Integer
    colorAt coord robotState = if coord `Set.member` whitePanels robotState then 1 else 0

    setColorAt :: Coord -> Integer -> RobotState -> RobotState
    setColorAt coord color robotState =
      robotState
        { whitePanels = case color of
            0 -> coord `Set.delete` whitePanels robotState
            1 -> coord `Set.insert` whitePanels robotState
            _ -> error "Processor output invalid color",
          touchedPanels = coord `Set.insert` touchedPanels robotState
        }

    turnRobot :: Integer -> RobotState -> RobotState
    turnRobot turnDirection robotState =
      robotState
        { robotDirection = case (turnDirection, robotDirection robotState) of
            (0, DirUp) -> DirLeft
            (0, DirRight) -> DirUp
            (0, DirDown) -> DirRight
            (0, DirLeft) -> DirDown
            (1, DirUp) -> DirRight
            (1, DirRight) -> DirDown
            (1, DirDown) -> DirLeft
            (1, DirLeft) -> DirUp
            _ -> error "Processor output invalid turn direction"
        }

    moveRobotForward :: RobotState -> RobotState
    moveRobotForward robotState =
      robotState {robotPosition = moveDirection (robotDirection robotState) (robotPosition robotState)}

moveDirection :: Direction -> Coord -> Coord
moveDirection DirUp (x, y) = (x, y + 1)
moveDirection DirRight (x, y) = (x + 1, y)
moveDirection DirDown (x, y) = (x, y - 1)
moveDirection DirLeft (x, y) = (x - 1, y)

--------------------------------------------------
-- Main intcode implementation

runIntcodeProgram :: Seq Integer -> Pipe Integer Integer Maybe ()
runIntcodeProgram initialMemory = initializeInterpreter initialMemory stepUntilHalt

stepUntilHalt :: Interpreter ()
stepUntilHalt = do
  pc <- getProgramCounter
  instruction <- getMemoryAt pc
  if instruction == 99
    then return () -- Halt
    else step >> stepUntilHalt

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
    let newMemory = mem >< Seq.replicate (fromInteger (address + 1 - memLength)) 0
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