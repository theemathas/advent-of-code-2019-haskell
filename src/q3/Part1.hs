import qualified Data.Set as Set (fromList, intersection)
import MyPrelude
import Relude.Extra.Foldable1 (Foldable1 (minimum1))
import Text.Parsec (char, choice, endBy, eof, newline, sepBy1)

data Direction = U | D | L | R
  deriving (Show, Eq)

type Coordinate = (Integer, Integer)

main :: IO ()
main = do
  input <- parseFromFileOrError parseInput "src/q3/input.txt"
  let [path1, path2] = input -- Assume that input has 2 lines
  let coordinates1 = Set.fromList $ coordinatesInPath path1
  let coordinates2 = Set.fromList $ coordinatesInPath path2
  -- Assume at least 1 intersection
  let Just intersections = nonEmpty $ toList $ Set.intersection coordinates1 coordinates2
  print $ minimum1 $ fmap (\(x, y) -> abs x + abs y) intersections

parseInput :: Parser [[(Direction, Integer)]]
parseInput = (parseLine `endBy` newline) <* eof
  where
    parseLine = parseSegment `sepBy1` char ','
    parseSegment = do
      direction <- choice [char 'U' $> U, char 'D' $> D, char 'L' $> L, char 'R' $> R]
      number <- unsignedDecimal
      return (direction, number)

coordinatesInPath :: [(Direction, Integer)] -> [Coordinate]
coordinatesInPath = helper (0, 0)
  where
    helper :: Coordinate -> [(Direction, Integer)] -> [Coordinate]
    helper _ [] = []
    helper startCoord ((direction, number) : restOfPath)
      | number < 0 = error "Negative segment"
      | number == 0 = helper startCoord restOfPath
      | otherwise =
        let (x, y) = startCoord
            nextCoord = case direction of
              U -> (x, y + 1)
              D -> (x, y - 1)
              L -> (x - 1, y)
              R -> (x + 1, y)
         in nextCoord : helper nextCoord ((direction, number - 1) : restOfPath)

-- if number < 0
--   then error "Negative segment"
--   else
--     if number == 0
--       then helper startCoord restOfPath
--       else undefined