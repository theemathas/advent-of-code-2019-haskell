import Control.Exception (assert)
import Data.List (elemIndex)
import MyPrelude
import Relude.Extra.Map ((!?))
import Text.Parsec

main :: IO ()
main = do
  input <- parseFromFileOrError parseInput "src/q6/input.txt"
  let parentOf = fromList (map swap input) :: Map Text Text
  let getParentPath node =
        case parentOf !? node of
          (Just parent) -> parent : getParentPath parent
          Nothing -> assert (node == "COM") []
  print $ pathLengthFromParentPaths (getParentPath "YOU") (getParentPath "SAN") - 2

parseInput :: Parser [(Text, Text)]
parseInput = do line `endBy1` newline
  where
    object = toText <$> many1 (oneOf (['A' .. 'Z'] ++ ['0' .. '9']))
    line = do
      object1 <- object
      _ <- char ')'
      object2 <- object
      return (object1, object2)

pathLengthFromParentPaths :: Eq a => [a] -> [a] -> Int
pathLengthFromParentPaths [] _ = error "wut"
pathLengthFromParentPaths (x : xs) ys =
  case elemIndex x ys of
    Just k -> 2 + k
    Nothing -> 1 + pathLengthFromParentPaths xs ys