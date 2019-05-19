{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Text.IO as TIO


main :: IO ()
main =
  do
    TIO.putStrLn "Please enter a username."
    username <- Username <$> TIO.getLine
    TIO.putStrLn "Please enter a password."
    password <- Password <$> TIO.getLine
    displayErrors username password


main2 :: IO ()
main2 =
  TIO.putStrLn "Please enter a password"
  >> TIO.getLine >>= (\pass -> print $ validatePassword $ Password pass)
