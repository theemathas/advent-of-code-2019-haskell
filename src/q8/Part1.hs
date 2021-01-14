import Data.Foldable (minimumBy)
import Data.Sequence (chunksOf)
import MyPrelude
import Text.Parsec (char, eof, many1, newline)

main :: IO ()
main = do
  digits <- parseFromFileOrError parseInput "src/q8/input.txt"
  print $ compute digits

parseInput :: Parser (Seq Integer)
parseInput = (fromList <$> many1 parseDigit) <* (newline >> eof)
  where
    parseDigit = (char '0' $> 0) <|> (char '1' $> 1) <|> (char '2' $> 2)

compute :: Seq Integer -> Integer
compute digits =
  let layers = chunksOf (6 * 25) digits
      -- Layer with most zeroes
      layerOfInterest = minimumBy (comparing (countInSeq 0)) layers
   in countInSeq 1 layerOfInterest * countInSeq 2 layerOfInterest

countInSeq :: Eq a => a -> Seq a -> Integer
countInSeq x xs = toInteger $ length $ filter (== x) $ toList xs
