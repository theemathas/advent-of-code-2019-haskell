import Data.Sequence (update, (!?))
import Data.Text (splitOn)
import Relude
import Relude.Unsafe (fromJust, read)

main :: IO ()
main = do
  input <- readFileText "src/q2/input.txt"
  let memory = fromList (read . toString <$> splitOn "," input) :: Seq Int
  -- Task tells us to do this for some reason
  let modifiedMemory = update 1 12 $ update 2 2 memory
  let result = fromJust $ runIntcode 0 modifiedMemory
  print $ fromJust $ result !? 0

runIntcode :: Int -> Seq Int -> Maybe (Seq Int)
runIntcode programCounter memory = do
  instruction <- memory !? programCounter
  case instruction of
    99 -> return memory
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
      runIntcode (programCounter + 4) newMemory