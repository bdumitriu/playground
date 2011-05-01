{-# LINE 11 "SLMain.lhs" #-}
import SLParser
import UU.Pretty
import UU.Parsing 

slVersion
  = "SL Interpreter"
slDate
  = "20031106"
slIntroducedFeatures
  = "Introduction to AG & Silly SL Interpreter"
slSignOn =  
  " ____   _"  ++ "\n" ++
  "/ ___| | |" ++ "\tSL (Simple Language) Compiler.\n" ++
  "\\___ \\" ++ " | | " ++ "\tVersion " ++ slVersion ++ "/" ++ slDate ++ "\n" ++    
  " ___) || |__  " ++ "\tIntroducedFeatures: " ++ "\n" ++  
  "|____/ |_____| " ++ "\t\t" ++ slIntroducedFeatures ++"\n"

compile filename = do 
  tokens <- slScan filename
  (exprpp,res) <- parseIO (pRoot ()) tokens
  putStrLn ("The expression:\n" ++ (disp exprpp 100 ""))   
  putStrLn "Evaluates to:"
  putStrLn (show res)
  putStrLn "----------------------------------------------------"
  
main = do
  putStrLn slSignOn
  putStr "Enter filename: "
  filename <- getLine
  tokens <- slScan filename
  compile filename
