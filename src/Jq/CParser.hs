module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- char '.'
  return Identity

combineFilters :: [String] -> Filter
combineFilters [] = Identity
combineFilters (x:xs) = PipeOperator (if last x /= '?' then IdentifierIndexing x else OptIdentifierIndexing x, combineFilters xs)

identifierIndexing :: Parser String
identifierIndexing = char '.' *> identifier <|> char '.' *> stringLiteral <|> char '.' *> char '[' *> stringLiteral <* char ']'

optIdentifierIndexing :: Parser String
optIdentifierIndexing = char '.' *> identifier <* char '?' <|> char '.' *> stringLiteral <* char '?' <|> char '.' *> char '[' *> stringLiteral <* char ']' <* char '?'

parseIdentifierIndexing :: Parser Filter
parseIdentifierIndexing = combineFilters <$> some (do s <- optIdentifierIndexing
                                                      return (s ++ "?")
                                                  <|> identifierIndexing)

valueIterator :: Parser [Int]
valueIterator = char '.' *> intArray


parseIndexingIterator :: Parser Filter
parseIndexingIterator = do  indexing <- parseIdentifierIndexing
                            _ <- string "[]"
                            return (PipeOperator (indexing, ValueIterator []))

sliceIterator :: Parser [Int]
sliceIterator = do  _ <- char '.'
                    _ <- char '['
                    _ <- space
                    l <- integer
                    _ <- space
                    _ <- char ':'
                    _ <- space
                    r <- integer
                    _ <- space
                    _ <- char ']'
                    return (createArrayFromSlice l r)

parseValueIterator :: Parser Filter
parseValueIterator =  ValueIterator <$> valueIterator

parseSliceIterator :: Parser Filter
parseSliceIterator = SliceIterator <$> sliceIterator

optValueIterator :: Parser [Int]
optValueIterator = char '.' *> intArray <* char '?'

optSliceIterator :: Parser [Int]
optSliceIterator = do _ <- char '.'
                      _ <- char '['
                      _ <- space
                      l <- integer
                      _ <- space
                      _ <- char ':'
                      _ <- space
                      r <- integer
                      _ <- space
                      _ <- char ']'
                      _ <- char '?'
                      return (createArrayFromSlice l r)

parseOptValueIterator :: Parser Filter
parseOptValueIterator = OptValueIterator <$> optValueIterator

parseOptSliceIterator :: Parser Filter
parseOptSliceIterator = OptSliceIterator <$> optSliceIterator

createArrayFromSlice :: Int -> Int -> [Int]
createArrayFromSlice l r = if l < r then l : createArrayFromSlice (l + 1) r else []

intArray :: Parser [Int]
intArray = char '[' *> space *> elements <* space <* char ']'
            where elements = (:) <$> integer <*> many ((space *> char ',' <* space) *> integer) <|> pure []

parseParenthesis :: Parser Filter
parseParenthesis = char '(' *> parseFilter <* char ')'

parseComaOperator :: Parser Filter
parseComaOperator = do  l <- parseFilterWithoutComaAndPipe
                        _ <- space
                        _ <- char ','
                        _ <- space
                        r <- parseFilter
                        return (ComaOperator (l, r))

parsePipeOperator :: Parser Filter
parsePipeOperator = do  l <- parseFilterWithoutComaAndPipe
                        _ <- space
                        _ <- char '|'
                        _ <- space
                        r <- parseFilter
                        return (PipeOperator (l, r))

parseJsonValue :: Parser Filter
parseJsonValue = JsonValue <$> parseJSON


parseArrayConstructor :: Parser Filter
parseArrayConstructor = do  _ <- char '['
                            f <- parseFilter
                            _ <- char ']'
                            return (ArrayConstructor f)

parseObjectConstructor :: Parser Filter
parseObjectConstructor = ObjectConstructor <$> (char '{' *> space *> ((:) <$> kvPair <*> many ((space *> char ',' <* space) *> kvPair) <|> pure []) <* space <* char '}')
                                                where
                                                  kvPair = (\k _ v -> (k, v)) <$> identifier <*> (space *> char ':' <* space) <*> parseFilter

parseFilterWithoutComaAndPipe :: Parser Filter
parseFilterWithoutComaAndPipe = token (
                parseParenthesis
                <|>
                parseIndexingIterator
                <|>
                parseIdentifierIndexing
                <|>
                parseOptSliceIterator
                <|>
                parseSliceIterator
                <|>
                parseOptValueIterator
                <|>
                parseValueIterator
                <|>
                parseIdentity
                <|>
                parseArrayConstructor
                <|>
                parseObjectConstructor
                <|>
                parseJsonValue
              )

parseFilter :: Parser Filter
parseFilter = token (
                parseComaOperator
                <|>
                parsePipeOperator
                <|>
                parseParenthesis
                <|>
                parseIndexingIterator
                <|>
                parseIdentifierIndexing
                <|>
                parseOptSliceIterator
                <|>
                parseSliceIterator
                <|>
                parseOptValueIterator
                <|>
                parseValueIterator
                <|>
                parseIdentity
                <|>
                parseArrayConstructor
                <|>
                parseObjectConstructor
                <|>
                parseJsonValue
              )

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
