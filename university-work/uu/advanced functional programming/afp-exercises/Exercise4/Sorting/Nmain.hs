import Nsort

main = do
  input <- getContents
  putStrLn (unwords (msort (words input)))
