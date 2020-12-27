import Relude
import Relude.Unsafe (read)

main :: IO ()
main = do
  input <- readFileText "src/q1/input.txt"
  let masses = (read . toString <$> lines input) :: [Integer]
  let fuels = map fuelFor masses
  unless (all (> 0) fuels) $ error "Negative fuel calculated"
  print $ sum fuels

fuelFor :: Integer -> Integer
fuelFor payloadMass =
  let fuel = payloadMass `div` 3 - 2
   in if fuel <= 0 then 0 else fuel + fuelFor fuel