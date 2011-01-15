module Main (main) where

import SimpleJSON
import PrettyJSON
import Prettify

sample1 :: JValue
sample1 = (JObject [("foo", JNumber 1), ("bar", JBool False)])

sample2 = (JArray [JObject [("foo", JNumber 1), ("bar", JBool False)], JString "hello world!"])

main = putStrLn . pretty 1 . indent 4 . renderJValue $ sample2
