module Jq.Filters where

import Jq.Json

data Filter = Identity | IdentifierIndexing String | ValueIterator [Int] | OptIdentifierIndexing String | OptValueIterator [Int] | SliceIterator [Int] | OptSliceIterator [Int] | ComaOperator (Filter, Filter) | PipeOperator (Filter, Filter) | JsonValue JSON | ArrayConstructor Filter | ObjectConstructor [(String, Filter)]
  deriving (Eq)

instance Show Filter where
  show Identity = "."
  show (IdentifierIndexing []) = ""
  show (IdentifierIndexing x) = '.' : x
  show (ValueIterator xs) = "." ++ "[" ++ show xs ++ "]"
  show (SliceIterator xs) = "." ++ "[" ++ show xs ++ "]"
  show (OptIdentifierIndexing []) = ""
  show (OptIdentifierIndexing x) = '.' : x ++ "?"
  show (OptValueIterator xs) = "." ++ "[" ++ show xs ++ "]?"
  show (OptSliceIterator xs) = "." ++ "[" ++ show xs ++ "]?"
  show (ComaOperator (l, r)) = show l ++ ", " ++ show r
  show (PipeOperator (l, r)) = show l ++ " | " ++ show r
  show (JsonValue json) = show json
  show (ArrayConstructor j) = show j
  show (ObjectConstructor xs) = "{" ++ concatMap showKeyValuePair xs ++ "}"

showKeyValuePair :: (String, Filter) -> String
showKeyValuePair (k, v) = "\"" ++ k ++ "\":" ++ show v

data Config = ConfigC {filters :: Filter}
