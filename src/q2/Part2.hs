import Data.Sequence (update, (!?))
import Data.Text (splitOn)
import Relude
import Relude.Unsafe (read)

main :: IO ()
main = do
  input <- readFileText "src/q2/input.txt"
  let memory = fromList (read . toString <$> splitOn "," input) :: Seq Int
  let result = [100 * noun + verb | noun <- [0 .. 99], verb <- [0 .. 99], runNounVerb memory noun verb == Just 19690720]
  print result

runNounVerb :: Seq Int -> Int -> Int -> Maybe Int
runNounVerb memory noun verb = do
  let modifiedMemory = update 1 noun $ update 2 verb memory
  runIntcode modifiedMemory 0

runIntcode :: Seq Int -> Int -> Maybe Int
runIntcode memory programCounter = do
  instruction <- memory !? programCounter
  case instruction of
    99 -> memory !? 0
    1 -> runOperator (+)
    2 -> runOperator (*)
    _ -> Nothing
  where
    runOperator op = do
      address1 <- memory !? (programCounter + 1)
      address2 <- memory !? (programCounter + 2)
      address3 <- memory !? (programCounter + 3)
      value1 <- memory !? address1
      value2 <- memory !? address2
      -- Just check if in bounds just in case
      _ <- memory !? address3
      let newMemory = update address3 (op value1 value2) memory
      runIntcode newMemory (programCounter + 4)