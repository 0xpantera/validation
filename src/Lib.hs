module Lib
    ( checkPasswordLength
    ) where

import Data.Char


someFunc :: IO ()
someFunc = putStrLn "someFunc"


checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password > 20) of
    True -> Nothing
    False -> Just password


requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    True -> Just xs
    False -> Nothing


    
