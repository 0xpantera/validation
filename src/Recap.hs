module Recap where



reverseLine :: IO ()
reverseLine =
  do
    line <- getLine
    print (reverse line)


bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a


data StringOrValue a = Str String | Val a deriving Show


bindStringOrValue
  :: StringOrValue a
  -> (a -> StringOrValue b)
  -> StringOrValue b
bindStringOrValue (Str s) f = Str s
bindStringOrValue (Val a) f = f a
