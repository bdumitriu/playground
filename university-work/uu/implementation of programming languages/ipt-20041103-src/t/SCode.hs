{-# LINE 7 "SCode.lhs" #-}
module SCode where 
import Prelude hiding (Ordering(..))

newtype Label = Label Int
instance Show Label where
  show (Label i) = 'L': (show i)

data S =
    HALT
  | TRAP        Int
  | AJS         Int
  | SWP
  | LDC         Int
  | LDLABEL     Label
  | LDS         Int 
  | ADD
  | SUB
  | MUL
  | DIV
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  | AND 
  | OR 
  | BRA         Label
  | BRF         Label 
  | DEFLABEL    Label 
  | ANNOTE      String Int Int String String 
  | LDMS        Int Int
  | LDL         Int
  | LDML        Int Int
  | LDR         String
  | LDRR        String String
  | LDA         Int
  | LDMA        Int Int
  | STS         Int
  | STMS        Int Int
  | STL         Int
  | STML        Int Int
  | STR         String
  | STA         Int
  | STMA        Int Int
  | JSR
  | RET
  

instance Show S where
  show s = case s of
    HALT               -> "  HALT"
    TRAP x             -> "  TRAP " ++ show x  
    AJS l              -> "  AJS  " ++ show l
    SWP                -> "  SWP"
    LDC c              -> "  LDC  " ++ show c
    LDLABEL l          -> "  LDC  " ++ show l
    LDS d              -> "  LDS  " ++ show d
    ADD                -> "  ADD"
    SUB                -> "  SUB"
    MUL                -> "  MUL"
    DIV                -> "  DIV"
    EQ                 -> "  EQ"
    NE                 -> "  NE"
    LT                 -> "  LT"
    GT                 -> "  GT"
    LE                 -> "  LE"
    GE                 -> "  GE"
    AND                -> "  AND"  
    OR                 -> "  OR"
    BRA l              -> "  BRA " ++ show l
    BRF l              -> "  BRF " ++ show l
    DEFLABEL l         -> show l ++ ": "
    ANNOTE r l h cl t  -> "  annote " ++ r ++ " " ++ show l ++ " " ++ show h ++ " " ++ cl ++ " " ++ show t
    LDMS d s           -> "  LDMS " ++ show d ++ " " ++ show s
    LDL  d             -> "  LDL  " ++ show d
    LDML d s           -> "  LDML " ++ show d ++ " " ++ show s
    LDR d              -> "  LDR  " ++ d
    LDRR r1 r2         -> "  LDRR " ++ r1 ++ " " ++ r2
    LDA d              -> "  LDA  " ++ show d 
    LDMA d s           -> "  LDMA " ++ show d ++ " " ++ show s
    STS d              -> "  STS  " ++ show d
    STMS d s           -> "  STMS " ++ show d ++ " " ++ show s
    STL d              -> "  STL  " ++ show d
    STML d s           -> "  STML " ++ show d ++ " " ++ show s
    STR r              -> "  STR  " ++ r
    STA d              -> "  STA  " ++ show d
    STMA d s           -> "  STMA " ++ show d ++ " " ++ show s
    JSR                -> "  JSR"
    RET                -> "  RET"

opCode :: String -> S
opCode op = case op of
  "&&"  -> AND
  "||"  -> OR
  "=="  -> EQ
  "/="  -> NE
  ">="  -> GE
  "<="  -> LE
  "<"   -> LT
  ">"   -> GT
  "+"   -> ADD
  "-"   -> SUB
  "*"   -> MUL
  "/"   -> DIV
