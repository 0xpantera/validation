module Lib
    ( checkPasswordLength
    ) where

import Data.Char


someFunc :: IO ()
someFunc = putStrLn "someFunc"


checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password > 20 || length password < 10) of
    True -> Nothing
    False -> Just password


requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    True -> Just xs
    False -> Nothing


cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Just (x : xs)


passwordValidation :: String -> Maybe String
passwordValidation password =
  case (cleanWhitespace password) of
    Nothing -> Nothing
    Just password2 ->
      case (requireAlphaNum password2) of
        Nothing -> Nothing
        Just password3 ->
          case (checkPasswordLength password3) of
            Nothing -> Nothing
            Just password4 -> Just password4
