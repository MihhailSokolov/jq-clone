module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Maybe
import Data.List
import Data.Either


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]
compile (IdentifierIndexing x) (JObject obj) = if length result > 0 then return result else return [JNull] where result = [snd kv | kv <- obj, fst kv == x]
compile (IdentifierIndexing _) JNull = return [JNull]
compile (IdentifierIndexing _) _ = Left "Incorrect type for indexing"
compile (OptIdentifierIndexing x) (JObject obj) = if length result > 0 then return result else return [JNull] where result = [snd kv | kv <- obj, fst kv == init x]
compile (OptIdentifierIndexing _) JNull = return [JNull]
compile (OptIdentifierIndexing _) _ = return []

compile (ValueIterator _) JNull = Left "Cannot iterate over null"
compile (ValueIterator []) (JString str) = return [JString str]
compile (ValueIterator xs) (JString str) = if correctIndexes xs str then return [JString [s | s <- str, index s str `elem` xs]] else return [JNull]
compile (ValueIterator []) (JArray js) = return js
compile (ValueIterator _) (JArray []) = return [JArray []]
compile (ValueIterator xs) (JArray js) = if correctIndexes xs js then return [JArray [j | j <- js, index j js `elem` xs]] else Left "Incorrect index"
compile (ValueIterator []) (JObject js) = return (map snd js)
compile (ValueIterator _) (JObject _) = Left "Cannot index object"
compile (ValueIterator _) _ = Left "Incorrect iterator type"

compile (OptValueIterator _) JNull = return []
compile (OptValueIterator []) (JString str) = return [JString str]
compile (OptValueIterator xs) (JString str) = return [JString [s | s <- str, index s str `elem` xs]]
compile (OptValueIterator []) (JArray js) = return js
compile (OptValueIterator _) (JArray []) = return [JArray []]
compile (OptValueIterator xs) (JArray js) = return [JArray [j | j <- js, index j js `elem` xs]]
compile (OptValueIterator []) (JObject js) = return (map snd js)
compile (OptValueIterator _) (JObject _) = return []
compile (OptValueIterator _) _ = return []

compile (SliceIterator _) JNull = Left "Cannot iterate over null"
compile (SliceIterator []) (JString str) = return [JString str]
compile (SliceIterator xs) (JString str) = if correctIndexes xs str then return [JString [s | s <- str, index s str `elem` xs]] else return [JNull]
compile (SliceIterator []) (JArray js) = return js
compile (SliceIterator _) (JArray []) = return [JArray []]
compile (SliceIterator xs) (JArray js) = if correctIndexes xs js then return [JArray [j | j <- js, index j js `elem` xs]] else Left "Incorrect index"
compile (SliceIterator []) (JObject js) = return (map snd js)
compile (SliceIterator _) (JObject _) = Left "Cannot index object"
compile (SliceIterator _) _ = Left "Incorrect iterator type"

compile (OptSliceIterator _) JNull = return [JNull]
compile (OptSliceIterator []) (JString str) = return [JString str]
compile (OptSliceIterator xs) (JString str) = return [JString [s | s <- str, index s str `elem` xs]]
compile (OptSliceIterator []) (JArray js) = return js
compile (OptSliceIterator _) (JArray []) = return [JArray []]
compile (OptSliceIterator xs) (JArray js) = return [JArray [j | j <- js, index j js `elem` xs]]
compile (OptSliceIterator []) (JObject js) = return (map snd js)
compile (OptSliceIterator _) (JObject _) = return []
compile (OptSliceIterator _) _ = return []

compile (ComaOperator (f, s)) j | isLeft first = first
                                | isLeft second = second
                                | isRight first && isRight second = Right (fromRight [] first ++ fromRight [] second)
                                | otherwise = Left "Error"
                                where
                                    first = compile f j
                                    second = compile s j

compile (PipeOperator (f, s)) j | isLeft first = first
                                | isRight first = Right (concatMap (fromRight [] . compile s) (fromRight [] first))
                                | otherwise = Left "Error"
                                where
                                    first = compile f j

compile (JsonValue json) _ = Right [json]

compile (ArrayConstructor f) j  | isRight result = Right [JArray (fromRight [] result)]
                                | otherwise = Right [JArray []]
                                where result = compile f j

compile (ObjectConstructor []) _ = Right [JObject []]
compile (ObjectConstructor [(k, v)]) j = Right [JObject [(k, val)] | val <- fromRight [] (compile v j)]
compile (ObjectConstructor ((k, v):kvs)) j = (++) <$> Right [JObject [(k, val)] | val <- fromRight [] (compile v j)] <*> compile (ObjectConstructor kvs) j

correctIndexes :: [Int] -> [a] -> Bool
correctIndexes (x:xs) arr = x >= 0 && x < length arr && correctIndexes xs arr
correctIndexes _ _ = True

index :: Eq a => a -> [a] -> Int
index x xs = fromMaybe (-1) $ elemIndex x xs


run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j