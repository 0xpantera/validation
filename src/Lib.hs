module Lib where

import Data.Char


newtype Password = Password String
  deriving Show

newtype Error = Error String
  deriving Show

newtype Username = Username String
  deriving Show


checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  case (length password > 20 || length password < 10) of
    True -> Left (Error "Your password has to be between 10 and 20 characters long")
    False -> Right (Password password)


checkUsernameLength :: String -> Either Error Username
checkUsernameLength name =
  case (length name > 15) of
    True -> Left (Error "Username cannot be longer than 15 characters.")
    False -> Right (Username name)


requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    True -> Right xs
    False -> Left (Error "Your password cannot contain whitespace or special characters.")


cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Your password cannot be empty")
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Right (x : xs)


validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
  cleanWhitespace password
  >>= requireAlphaNum
  >>= checkPasswordLength


