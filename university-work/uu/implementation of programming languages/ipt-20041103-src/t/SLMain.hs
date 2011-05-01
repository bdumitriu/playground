{-# LINE 6 "SLMain.lhs" #-}
import UU.Pretty
import UU.Parsing
import SLParser
import SLTypes
import SLAttributes

slVersion
  = "Code Generation"
slDate
  = "20031106"
slIntroducedFeatures
  = "Code Generation & Type Checking"
slSignOn =  
  " ____   _"  ++ "\n" ++
  "/ ___| | |" ++ "\tSL (Simple Language) Compiler.\n" ++
  "\\___ \\" ++ " | | " ++ "\tVersion " ++ slVersion ++ "/" ++ slDate ++ "\n" ++    
  " ___) || |__  " ++ "\tIntroducedFeatures: " ++ "\n" ++  
  "|____/ |_____| " ++ "\t\t" ++ slIntroducedFeatures ++"\n"

compile filename = do
  tokens <- slScan filename
  (codepp,exprpp,tp) <- parseIO (pRoot ()) tokens
  putStrLn ("Code generated for the expression:\n" ++ (disp exprpp 300 ""))
  let basename = takeWhile ((/=) '.') filename
  writeFile (basename ++ ".ssm") (disp codepp 80 "")
  putStrLn ("Generated code in file " ++ basename ++".ssm")
  putStrLn "----------------------------------------------------"
              
main = do
  putStrLn slSignOn
  putStr "Enter filename: "
  filename <- getLine
  compile filename
