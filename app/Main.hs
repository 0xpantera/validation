module Main where

import Lib

main :: IO ()
main =
  do
    putStrLn "Please enter a password"
    password <- getLine
    print (checkPasswordLength password)
