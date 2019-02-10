module Main where

import Lib


main :: IO ()
main =
  do
    putStrLn "Please enter a username."
    username <- Username <$> getLine
    putStrLn "Please enter a password."
    password <- Password <$> getLine
    print (makeUser username password)


main2 :: IO ()
main2 =
  putStrLn "Please enter a password"
  >> getLine >>= (\pass -> print $ validatePassword $ Password pass)
