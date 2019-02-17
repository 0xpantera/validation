module Lib where

import Data.Char


newtype Password = Password String
  deriving Show

newtype Error = Error String
  deriving (Show, Eq)

newtype Username = Username String
  deriving Show


data User = User Username Password
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


checkLength :: Int -> String -> Either Error String
checkLength n field =
  case (length field > n) of
    True -> Left (Error ("Fields cannot be longer than " ++ (show n) ++ " characters"))
    False -> Right field


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


validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  cleanWhitespace username
  >>= requireAlphaNum
  >>= checkUsernameLength


doValidateUsername :: Username -> Either Error Username
doValidateUsername (Username username) =
  do
    cleanUser <- cleanWhitespace username
    alphaNumUser <- requireAlphaNum cleanUser
    checkUsernameLength alphaNumUser


makeUser :: Username -> Password -> Either Error User
makeUser name pass =
  User <$> (validateUsername name)
       <*> (validatePassword pass)


makeUserTmpPassword :: Username -> Either Error User
makeUserTmpPassword name =
  User <$> validateUsername name
       <*> pure (Password "temporaryPassword")

