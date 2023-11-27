import System.Environment
import System.Exit
import System.IO

ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) (ack m (n - 1))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mstr, nstr] -> do
      let m = read mstr :: Integer
      let n = read nstr :: Integer
      putStrLn ("ack " ++ mstr ++ " " ++ nstr ++ " = " ++ show (ack m n))
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage " ++ name ++ " <m> <n>"
      exitFailure

