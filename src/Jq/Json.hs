module Jq.Json where

import Text.ParserCombinators.ReadP
import Data.Char (isPrint)
import Text.Read.Lex
import Control.Applicative
data JSON = JNull | JString String | JBool Bool | JArray [JSON] | JObject [(String, JSON)] | JChar Char | JNum Double
  deriving (Eq)

instance Show JSON where
  show = prettyPrint 0

prettyPrint :: Int -> JSON -> String
prettyPrint _ JNull = "null"
prettyPrint _ (JNum jn) = printNum jn
prettyPrint _ (JString str) = printString str
prettyPrint _ (JChar c) = "\"" ++ [c] ++ "\""
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

printString :: String -> String
printString str = f ("", "") $ readP_to_S (Text.ParserCombinators.ReadP.many $ recoverChar (\c -> isPrint c && notElem c ['\\', '\'','\"'])) (show str)
                  where
                    f :: (String, String) -> [([(String, String)], String)] -> String
                    f _  [] = ""
                    f _  (([],""):_) = ""
                    f _  ((rs,""):_) = snd $ last rs
                    f _  [(_,o)] = o
                    f pr (([],_):rest) = f pr rest
                    f _  ((rs,_):rest) = snd (last rs) ++ f (last rs) rest 

recoverChar :: (Char -> Bool) -> ReadP (String, String)
recoverChar p = (represent <$> gather lexChar) <|> (("\\&","\&") <$ string "\\&")
  where
    represent :: (String, Char) -> (String, String)
    represent (o,lc)
      | p lc      = (o, [lc])
      | otherwise = (o, o)