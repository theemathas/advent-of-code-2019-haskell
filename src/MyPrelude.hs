module MyPrelude
  ( module Relude,
    Parser,
    parseFromFileOrError,
    decimal,
  )
where

import Relude
import Relude.Unsafe (read)
import Text.Parsec.Char (oneOf)
import Text.Parsec.Text (Parser, parseFromFile)

parseFromFileOrError :: Parser a -> FilePath -> IO a
parseFromFileOrError parser filePath = do
  parseResult <- parseFromFile parser filePath
  return $ case parseResult of
    Left e -> error $ show e
    Right result -> result

decimal :: Parser Integer
decimal = do
  firstDigit <- oneOf ['1' .. '9']
  laterDigits <- many (oneOf ['0' .. '9'])
  let digits = firstDigit : laterDigits
  return $ read digits