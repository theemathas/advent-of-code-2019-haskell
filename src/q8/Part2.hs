import Data.Foldable (Foldable (foldr1))
import Data.Sequence (chunksOf)
import Data.Text (append)
import MyPrelude
import Text.Parsec (char, eof, many1, newline)

main :: IO ()
main = do
  digits <- parseFromFileOrError parseInput "src/q8/input.txt"
  putText $ compute digits

parseInput :: Parser (Seq Integer)
parseInput = (fromList <$> many1 parseDigit) <* (newline >> eof)
  where
    parseDigit = (char '0' $> 0) <|> (char '1' $> 1) <|> (char '2' $> 2)

compute :: Seq Integer -> Text
compute digits =
  let layers = chunksOf (6 * 25) digits :: Seq (Seq Integer)
      combinedLayer = foldr1 combineLayers layers
   in formatLayer combinedLayer

combineLayers :: Seq Integer -> Seq Integer -> Seq Integer
combineLayers topLayer bottomLayer
  | length topLayer /= length bottomLayer = error "Mismatched layer lengths"
  | otherwise = fromList $ zipWith combinePixel (toList topLayer) (toList bottomLayer)
  where
    combinePixel 2 bottomPixel = bottomPixel
    combinePixel topPixel _ = topPixel

formatLayer :: Seq Integer -> Text
formatLayer layer =
  let rows = chunksOf 25 layer :: Seq (Seq Integer)
      rowTexts = fmap (foldr append "" . fmap formatPixel) rows :: Seq Text
   in foldr append "" $ (`append` "\n") <$> rowTexts

formatPixel :: Integer -> Text
formatPixel 0 = " "
formatPixel 1 = "█" -- U+2588
formatPixel _ = error "Invalid pixel"