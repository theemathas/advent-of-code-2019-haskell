import MyPrelude
import Text.Parsec (endBy, eof, newline, string)

data Moon = Moon {getPosition :: Vector, getVelocity :: Vector}
  deriving (Show)

main :: IO ()
main = do
  initialMoons <- parseFromFileOrError parseInput "src/q12/input.txt"
  let finalMoons = foldr ($) initialMoons (replicate 1000 step)
  print $ energyOf finalMoons

parseInput :: Parser [Moon]
parseInput = (parseMoon `endBy` newline) <* eof
  where
    parseMoon = do
      x <- string "<x=" >> decimal
      y <- string ", y=" >> decimal
      z <- string ", z=" >> decimal
      _ <- string ">"
      return $ Moon (Vector x y z) zeroVector

step :: [Moon] -> [Moon]
step moons = map stepMoon moons
  where
    -- Velocity change that the thing at pos' causes on the thing at pos
    gravityOfPos pos pos' = zipByVector (\a b -> signum (b - a)) pos pos'

    gravityOfMoon moon moon' = gravityOfPos (getPosition moon) (getPosition moon')

    stepMoon moon =
      let gravities = map (gravityOfMoon moon) moons
          netGravity = foldl' addVectors zeroVector gravities
          newVelocity = addVectors (getVelocity moon) netGravity
          newPosition = addVectors (getPosition moon) newVelocity
       in Moon newPosition newVelocity

energyOf :: [Moon] -> Integer
energyOf = sum . map (\m -> potentialEnergyOf m * kineticEnergyOf m)
  where
    potentialEnergyOf moon = sumAbs $ getPosition moon
    kineticEnergyOf moon = sumAbs $ getVelocity moon
    sumAbs (Vector x y z) = abs x + abs y + abs z

-- Vectors

data Vector = Vector {getX :: Integer, getY :: Integer, getZ :: Integer}
  deriving (Show)

zipByVector :: (Integer -> Integer -> Integer) -> Vector -> Vector -> Vector
zipByVector f (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (f x1 x2) (f y1 y2) (f z1 z2)

addVectors :: Vector -> Vector -> Vector
addVectors = zipByVector (+)

zeroVector :: Vector
zeroVector = Vector 0 0 0