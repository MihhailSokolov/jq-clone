module Jq.Json where

data JSON = JNull | JString String | JBool Bool | JArray [JSON] | JObject [(String, JSON)] | JChar Char | JNum Double
  deriving (Eq)

instance Show JSON where
  show = prettyPrint 0

prettyPrint :: Int -> JSON -> String
prettyPrint _ JNull = "null"
prettyPrint _ (JNum jn) = printNum jn
prettyPrint _ (JString str) = show str
prettyPrint _ (JChar char) = "\"" ++ [char] ++ "\""
prettyPrint _ (JBool b) = if b then "true" else "false"
prettyPrint n (JArray arr) = if length arr > 0 then "[\n" ++ prettyPrintArray (n + 2) arr ++ "\n" ++ indentation n ++ "]" else "[]"
prettyPrint n (JObject obj) = if length obj > 0 then "{\n" ++ prettyPrintObject (n + 2) obj ++ "\n" ++ indentation n ++ "}" else "{}"

printNum :: Double -> String
printNum d = if drop (length s - 2) s == ".0" then takeWhile (/= '.') s else s
              where s = show d

indentation :: Int -> String
indentation 0 = ""
indentation n = " " ++ indentation (n-1)

prettyPrintObject :: Int -> [(String, JSON)] -> String
prettyPrintObject _ [] = ""
prettyPrintObject n [(k, v)] = indentation n ++ show k ++ ": " ++ prettyPrint n v
prettyPrintObject n ((k, v):xs) = indentation n ++ show k ++ ": " ++ prettyPrint n v  ++ ",\n" ++ prettyPrintObject n xs

prettyPrintArray :: Int -> [JSON] -> String
prettyPrintArray _ [] = ""
prettyPrintArray n [j] = indentation n ++ prettyPrint n j
prettyPrintArray n (j:js) = indentation n ++ prettyPrint n j ++ ",\n" ++ prettyPrintArray n js