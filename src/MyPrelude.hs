module MyPrelude
  ( module Relude,
    Parser,
    parseFromFileOrError,
    unsignedDecimal,
    decimal,
  )
where

import Relude
import Relude.Unsafe (read)
import Text.Parsec (char, oneOf)
import Text.Parsec.Text (Parser, parseFromFile)

parseFromFileOrError :: Parser a -> FilePath -> IO a
parseFromFileOrError parser filePath = do
  parseResult <- parseFromFile parser filePath
  return $ case parseResult of
    Left e -> error $ show e
    Right result -> result

unsignedDecimal :: Parser Integer
unsignedDecimal =
  (char '0' $> 0) <|> do
    firstDigit <- oneOf ['1' .. '9']
    laterDigits <- many (oneOf ['0' .. '9'])
    let digits = firstDigit : laterDigits
    return $ read digits

decimal :: Parser Integer
decimal = do
  signFactor <- (char '-' $> -1) <|> (optional (char '+') $> 1)
  number <- unsignedDecimal
  return $ signFactor * number