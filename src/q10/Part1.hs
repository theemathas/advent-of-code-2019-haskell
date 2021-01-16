import Data.Foldable (maximum, maximumBy)
import Data.List (zipWith3)
import MyPrelude
import Text.Parsec (char, endBy1, eof, many1, newline)

-- (column, row)
type Coord = (Integer, Integer)

main :: IO ()
main = do
  grid <- parseFromFileOrError parseInput "src/q10/input.txt"
  let coords = gridToCoords grid
  print $ maximum $ map (`numDetectFrom` coords) coords
  print $ maximumBy (comparing (`numDetectFrom` coords)) coords

parseInput :: Parser [[Bool]]
parseInput = (parseLine `endBy1` newline) <* eof
  where
    parseLine = many1 ((char '#' $> True) <|> (char '.' $> False))

-- Given a grid of booleans, returns coordinates of elements that are true.
gridToCoords :: [[Bool]] -> [Coord]
gridToCoords grid = concat $ zipWith processRow [0 ..] grid
  where
    processRow :: Integer -> [Bool] -> [Coord]
    processRow rowIndex rowContents = concat $ zipWith3 processElem (repeat rowIndex) [0 ..] rowContents
    processElem :: Integer -> Integer -> Bool -> [Coord]
    processElem rowIndex colIndex True = [(colIndex, rowIndex)]
    processElem _ _ False = []

numDetectFrom :: Coord -> [Coord] -> Integer
numDetectFrom station asteroids = numDetectFromDiffs $ map (`subCoord` station) asteroids
  where
    numDetectFromDiffs :: [Coord] -> Integer
    numDetectFromDiffs diffs = toInteger (length (go [] diffs)) - 1

    -- go pastDiffs diffs attempts to add as many diffs to pastDiffs as possibly
    -- while not blocking each other, then returns the result.
    go :: [Coord] -> [Coord] -> [Coord]
    go pastDiffs (x : xs)
      | any (wouldBlock x) pastDiffs = go pastDiffs xs
      | otherwise = go (x : pastDiffs) xs
    go pastDiffs [] = pastDiffs

wouldBlock :: Coord -> Coord -> Bool
wouldBlock (x1, y1) (x2, y2) =
  (signum x1 == signum x2) && (signum y1 == signum y2) && (x1 * y2 == x2 * y1)

subCoord :: Coord -> Coord -> Coord
subCoord (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)