import Control.Exception (assert)
import qualified Data.HashMap.Strict as HM
import MyPrelude
import Text.Parsec (endBy, eof, newline, string)

-- TODO make this fast enough.

data MoonAxis = MoonAxis {getPosition :: Integer, getVelocity :: Integer}
  deriving (Show, Eq, Generic)

instance Hashable MoonAxis

data Moon = Moon {getX :: MoonAxis, getY :: MoonAxis, getZ :: MoonAxis}
  deriving (Show, Eq, Generic)

instance Hashable Moon

main :: IO ()
main = do
  initialMoons <- parseFromFileOrError parseInput "src/q12/input.txt"
  let xRepeat = findRepeat $ iterate stepMoonAxes $ map getX initialMoons
  let yRepeat = findRepeat $ iterate stepMoonAxes $ map getY initialMoons
  let zRepeat = findRepeat $ iterate stepMoonAxes $ map getZ initialMoons
  -- In our data, the repeat seems to always happen by going back to the initial
  -- configuration. This simplifies the computation a bit.
  assert (fst xRepeat == 0) (return ())
  assert (fst yRepeat == 0) (return ())
  assert (fst zRepeat == 0) (return ())
  print $ foldl' lcm 1 $ map snd [xRepeat, yRepeat, zRepeat]

parseInput :: Parser [Moon]
parseInput = (parseMoon `endBy` newline) <* eof
  where
    parseMoon = do
      x <- string "<x=" >> decimal
      y <- string ", y=" >> decimal
      z <- string ", z=" >> decimal
      _ <- string ">"
      return $ Moon (MoonAxis x 0) (MoonAxis y 0) (MoonAxis z 0)

-- Returns (index1, index2) where element at index1 is equal to element at index2
findRepeat :: (Hashable a, Eq a) => [a] -> (Integer, Integer)
findRepeat = go HM.empty 0
  where
    -- visited maps state -> index
    go :: (Hashable a, Eq a) => HashMap a Integer -> Integer -> [a] -> (Integer, Integer)
    go visited currentIndex (x : xs) =
      case HM.lookup x visited of
        Just previousIndex -> (previousIndex, currentIndex)
        Nothing -> go (HM.insert x currentIndex visited) (currentIndex + 1) xs
    go _ _ [] = error "Ran out of elements"

stepMoonAxes :: [MoonAxis] -> [MoonAxis]
stepMoonAxes moonAxes = map stepOneMoonAxis moonAxes
  where
    -- Velocity change that moonAxis' causes on moonAxis
    gravityOfMoonAxis :: MoonAxis -> MoonAxis -> Integer
    gravityOfMoonAxis moonAxis moonAxis' = signum (getPosition moonAxis' - getPosition moonAxis)

    stepOneMoonAxis :: MoonAxis -> MoonAxis
    stepOneMoonAxis moonAxis =
      let gravities = map (gravityOfMoonAxis moonAxis) moonAxes
          netGravity = sum gravities
          newVelocity = getVelocity moonAxis + netGravity
          newPosition = getPosition moonAxis + newVelocity
       in MoonAxis newPosition newVelocity