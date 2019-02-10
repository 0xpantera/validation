module Main where

import Lib


main :: IO ()
main =
  do
    putStrLn "Please enter a password"
    password <- Password <$> getLine
    print (validatePassword password)


main2 :: IO ()
main2 =
  putStrLn "Please enter a password"
  >> getLine >>= (\pass -> print $ validatePassword $ Password pass)
