{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
module Lib where

import Data.Char
import Data.Validation
import Data.Semigroup


newtype Password = Password String
  deriving Show


newtype Error = Error [String]
  deriving (Show, Semigroup)


newtype Username = Username String
  deriving Show


data User = User Username Password
  deriving Show


checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
  case (length password > 20 || length password < 10) of
    True -> Failure (Error ["Your password has to be between 10 and 20 characters long"])
    False -> Success (Password password)


checkUsernameLength :: String -> Validation Error Username
checkUsernameLength name =
  case (length name > 15) of
    True -> Failure (Error ["Username cannot be longer than 15 characters."])
    False -> Success (Username name)


checkLength :: Int -> String -> Validation Error String
checkLength n field =
  case (length field > n) of
    True -> Failure (Error ["Fields cannot be longer than " ++ (show n) ++ " characters"])
    False -> Success field


requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    True -> Success xs
    False -> Failure (Error ["Cannot contain whitespace or special characters."])


cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Cannot be empty"])
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True -> cleanWhitespace xs
    False -> Success (x : xs)


validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case (cleanWhitespace password) of
    Failure err -> Failure err
    Success password2 -> requireAlphaNum password2 *>
                         checkPasswordLength password2


validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case (cleanWhitespace username) of
    Failure err -> Failure err
    Success username2 -> requireAlphaNum username2 *>
                         checkUsernameLength username2


--doValidateUsername :: Username -> Validation Error Username
--doValidateUsername (Username username) =
--  do
--    cleanUser <- cleanWhitespace username
--    alphaNumUser <- requireAlphaNum cleanUser
--    checkUsernameLength alphaNumUser


makeUser :: Username -> Password -> Validation Error User
makeUser name pass =
  User <$> (validateUsername name)
       <*> (validatePassword pass)


makeUserTmpPassword :: Username -> Validation Error User
makeUserTmpPassword name =
  User <$> validateUsername name
       <*> pure (Password "temporaryPassword")


passwordErrors :: Password -> Validation Error Password
passwordErrors password =
  case validatePassword password of
    Failure err -> Failure (Error ["Invalid password:"]
                            <> err)
    Success password2 -> Success password2


usernameErrors :: Username -> Validation Error Username
usernameErrors username =
  case validateUsername username of
    Failure err -> Failure (Error ["Invalid username:"]
                            <> err)
    Success username2 -> Success username2
