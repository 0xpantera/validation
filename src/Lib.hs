module Lib
    ( checkPasswordLength
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password > 20) of
    True -> Nothing
    False -> Just password


    
