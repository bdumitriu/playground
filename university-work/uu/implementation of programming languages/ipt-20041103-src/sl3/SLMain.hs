{-# LINE 6 "SLMain.lhs" #-}
import SLParser
import UU.Pretty
import UU.Parsing
import SLTypes

import SLAttributes

slVersion
  = "Type Inferencing"
slDate
  = "20031106"
slIntroducedFeatures
  = "Type Inferencing"
slSignOn =  
  " ____   _"  ++ "\n" ++
  "/ ___| | |" ++ "\tSL (Simple Language) Compiler.\n" ++
  "\\___ \\" ++ " | | " ++ "\tVersion " ++ slVersion ++ "/" ++ slDate ++ "\n" ++    
  " ___) || |__  " ++ "\tIntroducedFeatures: " ++ "\n" ++  
  "|____/ |_____| " ++ "\t\t" ++ slIntroducedFeatures ++"\n"

compile filename = do
  tokens <- slScan filename
  (exprpp,ppTex,tp) <- parseIO (pRoot ()) tokens
  putStrLn ("The expression:\n" ++ (disp exprpp 300 ""))
  putStrLn ("Is inferred to have type " ++ show tp)
  let basename = takeWhile ((/=) '.') filename
  writeFile (basename ++ ".tex") (disp ppTex 200 "")
  putStrLn ("Generated proof deduction in file " ++ basename ++".tex")
  putStrLn "----------------------------------------------------"
              
main = do
  putStr slSignOn
  putStr "Enter filename: "
  filename <- getLine
  compile filename
