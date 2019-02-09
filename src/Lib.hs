module Lib
    ( validatePassword
    ) where

import Data.Char


checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case (length password > 20 || length password < 10) of
    True -> Left "Your password has to be between 10 and 20 characters long"
    False -> Right password


requireAlphaNum :: String -> Either String String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    True -> Right xs
    False -> Left "Your password cannot contain whitespace or special characters."


cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Your password cannot be empty"
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Right (x : xs)


validatePassword :: String -> Either String String
validatePassword password =
  cleanWhitespace password
  >>= requireAlphaNum
  >>= checkPasswordLength


