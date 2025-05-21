{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- JsonPaser
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module JsonParser where
import ParseFile (Parser, parseChar, parseAnyChar, parseMany, parseSome)
import Control.Applicative (Alternative(..), (<|>))


-- JSON data type
data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Double
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

-- parser for whitespace
parseWhitespace :: Parser String
parseWhitespace = parseMany (parseAnyChar " \t\n\r")

-- parse a token and skip trailing whitespace
parseToken :: Parser a -> Parser a
parseToken p = p <* parseWhitespace

-- JSON value parsers
parseJsonNull :: Parser JsonValue
parseJsonNull =
  JsonNull <$
  parseToken (parseChar 'n' *> parseChar 'u' *> parseChar 'l' *> parseChar 'l')

parseJsonBool :: Parser JsonValue
parseJsonBool =
  (JsonBool True <$ parseToken
  (parseChar 't' *> parseChar 'r' *> parseChar 'u' *> parseChar 'e')) <|>
  (JsonBool False <$ parseToken
  (parseChar 'f' *> parseChar 'a' *> parseChar 'l' *>
  parseChar 's' *> parseChar 'e'))

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

parseJsonNumber :: Parser JsonValue
parseJsonNumber = do
  _ <- parseWhitespace
  sign <- (parseChar '-' >> return (-1)) <|> return 1
  intStr <- parseSome parseDigit
  fracStr <- (parseChar '.' >> parseSome parseDigit) <|> return ""
  let intVal = read intStr :: Double
      fracVal = if null fracStr then 0 else read fracStr / (10^length fracStr)
      value = sign * (intVal + fracVal)
  _ <- parseWhitespace
  return $ JsonNumber value


parseStringChar :: Parser Char
parseStringChar = parseEscapeSequence <|> parseNormalChar
  where
    parseEscapeSequence = parseChar '\\' *> parseAnyChar "\\\"/bfnrt"
    parseNormalChar = parseAnyChar (filter (/= '"') [' '..'~'])

parseJsonString :: Parser JsonValue
parseJsonString = JsonString <$> parseToken (
  parseChar '"' *> parseMany parseStringChar <* parseChar '"'
  )

-- forward declaration for the recursive types
parseJsonValue :: Parser JsonValue
parseJsonValue = parseWhitespace *> (
  parseJsonNull <|>
  parseJsonBool <|>
  parseJsonNumber <|>
  parseJsonString <|>
  parseJsonArray <|>
  parseJsonObject
  )

parseJsonArray :: Parser JsonValue
parseJsonArray = JsonArray <$> parseToken (
  parseChar '[' *> parseWhitespace *>
  (parseJsonValue `sepBy` (parseWhitespace *>
  parseChar ',' <* parseWhitespace)) <*
  parseChar ']'
  )
  where
    sepBy :: Parser a -> Parser b -> Parser [a]
    sepBy p sep = ((:) <$> p <*> parseMany (sep *> p)) <|> pure []

-- helper function to separate key-value pairs
parseKeyValue :: Parser (String, JsonValue)
parseKeyValue = do
  key <- parseToken (parseChar '"' *> parseMany parseStringChar<*parseChar '"')
  _ <- parseToken (parseChar ':')
  value <- parseJsonValue
  return (key, value)

parseJsonObject :: Parser JsonValue
parseJsonObject = JsonObject <$> parseToken (
  parseChar '{' *> parseWhitespace *>
  (parseKeyValue `sepBy` (parseWhitespace *>
  parseChar ',' <* parseWhitespace)) <* parseChar '}'
  )
  where
    sepBy :: Parser a -> Parser b -> Parser [a]
    sepBy p sep = ((:) <$> p <*> parseMany (sep *> p)) <|> pure []