module TinyTests where

import Lib


-- takes a test result and prints it nicely to the terminal
printTestResult :: Either Error () -> IO ()
printTestResult r =
  case r of
    Left err -> print err
    Right () -> putStrLn "All tests passed."


-- represents an assertion that two values ought to be the same
eq :: (Eq a, Show a) => Int -> a -> a -> Either Error ()
eq n actual expected =
  case (actual == expected) of
    True -> Right ()
    False -> Left (unlines
                 [ "Test " ++ show n
                 , "  Expected:  " ++ show expected
                 , "  But got:  " ++ show actual
                 ])


test :: IO ()
test = printTestResult $
  do
    eq 1 (checkPasswordLength "123")
      (Left (Error "Your password has to be between 10 and 20 characters long"))
    eq 2 (checkPasswordLength "franklovesbooks")
      (Right (Password "franklovesbooks"))
    eq 3 (requireAlphaNum "this has spaces")
      (Left (Error "Your password cannot contain whitespace or special characters."))
    eq 4 (requireAlphaNum "thishasnospace123")
      (Right "thishasnospace123")
    eq 5 (cleanWhitespace "")
      (Left (Error "Your password cannot be empty"))
    eq 6 (cleanWhitespace " frankloves123")
      (Right "frankloves123")
