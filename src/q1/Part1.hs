import Relude
import Relude.Unsafe (read)

main :: IO ()
main = do
  input <- readFileText "src/q1/input.txt"
  let masses = (read . toString <$> lines input) :: [Integer]
  let fuels = map (\mass -> mass `div` 3 - 2) masses
  unless (all (> 0) fuels) $ error "Negative fuel calculated"
  print $ sum fuels