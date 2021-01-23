import qualified Data.HashSet as HS
import MyPrelude
import Text.Parsec (endBy, eof, newline, string)

-- TODO make this fast enough.

data Moon = Moon {getPosition :: Vector, getVelocity :: Vector}
  deriving (Show, Eq, Generic)

instance Hashable Moon

main :: IO ()
main = do
  initialMoons <- parseFromFileOrError parseInput "src/q12/input.txt"
  let moonSteps = iterate step initialMoons
  print $ findRepeat moonSteps

parseInput :: Parser [Moon]
parseInput = (parseMoon `endBy` newline) <* eof
  where
    parseMoon = do
      x <- string "<x=" >> decimal
      y <- string ", y=" >> decimal
      z <- string ", z=" >> decimal
      _ <- string ">"
      return $ Moon (Vector x y z) zeroVector

findRepeat :: (Hashable a, Eq a) => [a] -> Integer
findRepeat = go HS.empty
  where
    go :: (Hashable a, Eq a) => HashSet a -> [a] -> Integer
    go visited (x : xs)
      | HS.member x visited = 0
      | otherwise = 1 + go (HS.insert x visited) xs
    go _ [] = error "Ran out of elements"

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

-- Vectors

data Vector = Vector {getX :: Integer, getY :: Integer, getZ :: Integer}
  deriving (Show, Eq, Generic)

instance Hashable Vector

zipByVector :: (Integer -> Integer -> Integer) -> Vector -> Vector -> Vector
zipByVector f (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (f x1 x2) (f y1 y2) (f z1 z2)

addVectors :: Vector -> Vector -> Vector
addVectors = zipByVector (+)

zeroVector :: Vector
zeroVector = Vector 0 0 0