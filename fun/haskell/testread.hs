main :: IO ()
main = do
  case [1] of
    [1] -> do
      let xstr = "272"
      let x = read xstr :: Integer
      let y = read "123" :: Integer
      print x
      print y
    _ -> do
      putStrLn "nuttin."

