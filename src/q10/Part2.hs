import Control.Exception (assert)
import Data.List (groupBy, zipWith3)
import MyPrelude
import Relude.Unsafe (fromJust)
import Text.Parsec (char, endBy1, eof, many1, newline)

-- (column, row)
-- Positive x is to the right
-- Positive y is upwards
type Coord = (Integer, Integer)

stationCoord :: Coord
stationCoord = (22, 19) -- Output from part 1
-- TODO delete
-- stationCoord = (8, 3)

main :: IO ()
main = do
  grid <- parseFromFileOrError parseInput "src/q10/input.txt"
  let coords = gridToCoords grid
  let asteroids = assert (stationCoord `elem` coords) (filter (/= stationCoord) coords)
  let astroidSequence = peel (sortGrouped asteroids)
  let (x, y) = fromJust (astroidSequence !!? 199)
  print $ x * 100 + y

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

-- Sort coords by compareAngels. The coords with the same angle are grouped into
-- a single NonEmpty list, which is then sorted by compareDistances.
sortGrouped :: [Coord] -> [NonEmpty Coord]
sortGrouped coords = map (fromJust . nonEmpty . sortBy compareDistances) grouped
  where
    sorted = sortBy compareAngles coords
    grouped = groupBy (\x y -> compareAngles x y == EQ) sorted

-- Outputs the 1st element of each element, then the 2nd element of each
-- element, then the 3rd, etc.
peel :: [NonEmpty a] -> [a]
peel [] = []
peel xs = firstOfEach ++ peel remaining
  where
    firstOfEach = map head xs
    remaining = mapMaybe (nonEmpty . tail) xs

-- Ordered by: which astroid will be hit first by a laser from the station that
-- rotates clockwise, starting from going up. (starts with going towards
-- negative y, then rotating to positive x)
compareAngles :: Coord -> Coord -> Ordering
compareAngles coord1 coord2 =
  if segment1 /= segment2
    then compare segment1 segment2
    else compare (x2 * y1) (x1 * y2)
  where
    diff1 = coord1 `subCoord` stationCoord
    diff2 = coord2 `subCoord` stationCoord
    segment1 = segmentOf diff1
    segment2 = segmentOf diff2
    (x1, y1) = diff1
    (x2, y2) = diff2

    -- Divide the plane (except the origin) into 4 "segments"
    segmentOf :: Coord -> Integer
    segmentOf (0, y) | y < 0 = 0
    segmentOf (x, _) | x > 0 = 1
    segmentOf (0, y) | y > 0 = 2
    segmentOf (x, _) | x < 0 = 3
    segmentOf (0, 0) = error "Attempted to compare stationCoord"
    segmentOf _ = error "Should be unreachable"

-- Ordered by: which astroid is nearer to the station
compareDistances :: Coord -> Coord -> Ordering
compareDistances = comparing distanceOf
  where
    distanceOf coord = squareSize (coord `subCoord` stationCoord)
    squareSize (x, y) = x * x + y * y

subCoord :: Coord -> Coord -> Coord
subCoord (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)