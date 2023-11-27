main = do
  putStrLn "hello"
  putStrLn ("odds: " ++ show (filter odd [10 .. 20]))
