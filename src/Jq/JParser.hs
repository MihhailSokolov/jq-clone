module Jq.JParser where

import Parsing.Parsing
import Jq.Json

parseNull :: Parser JSON
parseNull = do _ <- string "null"
               return JNull

parseBool :: Parser JSON
parseBool = JBool True <$ string "true"
            <|>
            JBool False <$ string "false"

parseNum :: Parser JSON
parseNum = JNum <$> double

parseString :: Parser JSON
parseString = JString <$> stringLiteral

array :: Parser [JSON]
array = char '[' *> space *> elements <* space <* char ']'
            where elements = (:) <$> parseJSON <*> many ((space *> char ',' <* space) *> parseJSON) <|> pure []

parseArray :: Parser JSON
parseArray = JArray <$> array

parseObject :: Parser JSON
parseObject = JObject <$> (char '{' *> space *> ((:) <$> kvPair <*> many ((space *> char ',' <* space) *> kvPair) <|> pure []) <* space <* char '}')
                  where kvPair = (\k _ v -> (k, v)) <$> (char '\"' *> ident <* char '\"') <*> (space *> char ':' <* space) <*> parseJSON

parseJSON :: Parser JSON
parseJSON = token (parseNull <|> parseBool <|> parseNum <|> parseString <|> parseArray <|> parseObject)