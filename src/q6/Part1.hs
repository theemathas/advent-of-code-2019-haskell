import Control.Exception (assert)
import MyPrelude
import Relude.Extra.Map (keys, (!?))
import Text.Parsec

main :: IO ()
main = do
  input <- parseFromFileOrError parseInput "src/q6/input.txt"
  let parentOf = fromList (map swap input) :: Map Text Text
  let allNodes = keys parentOf
  let getParentPath node =
        case parentOf !? node of
          (Just parent) -> parent : getParentPath parent
          Nothing -> assert (node == "COM") []
  print $ sum [length (getParentPath node) | node <- allNodes]

parseInput :: Parser [(Text, Text)]
parseInput = do line `endBy1` newline
  where
    object = toText <$> many1 (oneOf (['A' .. 'Z'] ++ ['0' .. '9']))
    line = do
      object1 <- object
      _ <- char ')'
      object2 <- object
      return (object1, object2)