module UU_Scanner where

import Char
--import UU_Parsing_Symbol
-- old import UU_Parsing
import UU_BinaryTrees
import UU_Parsing_Core
import UU_Parsing_Derived

--import Trace

{- A parametrisable scanner
 -
 - Author: Doaitse Swierstra: doaitse@cs.uu.nl
      and: Pablo Azero      : pablo@cs.uu.nl
 - Version 1.0 , May 25, 1998, SDS
    first appearance on the software web site.
 - Version 1.01, June 7, 1998, SDS
    changed String recognition to recognise escaped characters
 - Version 1.02, Aug 30, 1998, SDS
    includes with unsafePerformIO
 - Version 2.1,  Jul  7, 1999, slightly different definition of token
                               ordering between tokens introduced
 - Version 2.2,  Jul  8, 1999, AG_Scanner and UU_Scanner merged
 - Version 2.3,  Jul 15, 1999, modifications: recognize decimal, octal and
 -                             hexadecimal numbers; handles ' as part of a
 -                             lower case identifier
 -                             fixes: bug in msort (loops when passing an
 -                             empty list)
 - Version 2.4,  Jul 23, 1999, additions: recognize characters and infix
 -                             operators
 -
 - Lang. compat: Hugs 98 (because it is required by UU_Parsing)
 - Version 2.5,  Aug 15, 1999, changed names, pSym -> pSpec
                             , all parsers start with p....
 - Version 2.6,  Sept 15, 1999, changed error message for unterminated string
 - Version 2.7,  Sept 23, 1999, changed definition of pOper_Any
 - Version 2.8   Aug 14,  2000, adapted to changes in search trees
 - ??            Oct 25,  2000, adapted to use column numbers
 - ??            Feb 2,   2001, incorporated changes of AD
 -}

data TokenType
  = TkSymbol
  | TkVarid
  | TkConid
  | TkKeyword
  | TkOp
  | TkString
  | TkChar
  | TkInteger8
  | TkInteger10
  | TkInteger16
  | TkTextnm
  | TkTextln
  | TkError
  deriving (Eq, Ord)

type Linenumber = (Int,Int)
type Filename   = String

newtype Token = Tok (TokenType, String, String, Linenumber, Filename)

instance Eq Token where
  Tok (ttypel    , stringl, _, _, _ ) == Tok (ttyper    , stringr, _,_ , _) =  ttypel == ttyper && stringl == stringr

instance   Ord Token where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  Tok (ttypel    , stringl, _, _, _ ) <= Tok (ttyper   , stringr, _,_ , _ )
      =     ttypel <  ttyper
        || (ttypel == ttyper && stringl <= stringr)

maybeshow :: Linenumber -> Filename -> String
maybeshow (0,0) fn = ""
maybeshow (l,c) fn = " at line " ++ show l
                       ++ ", column " ++ show c
                       ++ " of file " ++ show fn ++ "\n"

advl :: Linenumber -> Int -> Linenumber
advl (l,c) i = (l+i,1)

advc :: Linenumber -> Int -> Linenumber
advc (l,c) i = (l,c+i)

instance Show Token where
  showsPrec _ token
    = showString
       (case token of
        Tok (TkSymbol   , s1,  _, i, fn)  -> "Symbol   "              ++ s1 ++  maybeshow i fn
        Tok (TkOp       , s1,  _, i, fn)  -> "Op      "               ++ s1 ++  maybeshow i fn
        Tok (TkKeyword  , s1,  _, i, fn)  -> "Keyword "               ++ s1 ++  maybeshow i fn
        Tok (TkString   ,  _, s2, i, fn)  -> "String \""              ++ s2 ++ "\"" ++ maybeshow i fn
        Tok (TkChar     ,  _, s2, i, fn)  -> "Char '"                 ++ s2 ++ "'"  ++ maybeshow i fn
        Tok (TkInteger8 ,  _, s2, i, fn)  -> "Octal integer "         ++ s2 ++  maybeshow i fn
        Tok (TkInteger10,  _, s2, i, fn)  -> "Decimal Integer "       ++ s2 ++  maybeshow i fn
        Tok (TkInteger16,  _, s2, i, fn)  -> "Hexadecimal integer "   ++ s2 ++  maybeshow i fn
        Tok (TkVarid    ,  _, s2, i, fn)  -> "Lower case identifier " ++ s2 ++  maybeshow i fn
        Tok (TkConid    ,  _, s2, i, fn)  -> "Upper case identifier " ++ s2 ++  maybeshow i fn
        Tok (TkTextnm   ,  _, s2, i, fn)  -> "Text name "             ++ s2 ++  maybeshow i fn
        Tok (TkTextln   ,  _, s2, i, fn)  -> "Text line "             ++ s2 ++ maybeshow i fn
        Tok (TkError    ,  _, s2, i, fn)  -> "error in scanner: "     ++ s2 ++  maybeshow i fn
        )

instance  Symbol Token
--  where
--  del_pen (Tok (TkSymbol, "}", _, _, _)) = 10   -- ks: changed for testing
--  del_pen _                              = 5    -- standard value

isspace c  =  c == ' '  || c == '\t' || c == '\r'
           || c == '\f' || c == '\v'

scanner :: Bool -> [String] -> [String] -> String -> String -> String  -> Maybe String -> IO [Token]
scanner litmode keywordstxt keywordsops specchars opchars fn startstring
  = if litmode
    then case startstring
         of Nothing    -> topscan fn
            Just input -> do tokens <- tokenize ('\n':input) (0,1) fn
                             return tokens
    else case startstring
         of Nothing    -> codescan fn
            Just input -> do let (tokens,_,_) = codelines input (1,1) fn
                             return tokens
 where
   locatein t = let loc = btLocateIn compare.tab2tree.msort.map (:[]) $ t
                in  \v -> case loc v of{ Just _ -> True ; Nothing -> False}

   -- ks: changed each of the following four functions from
   -- locatein (zip xyz xyz)    to    locatein xyz
   iskw     = locatein keywordstxt
   isop     = locatein keywordsops
   isSymbol = locatein specchars
   isOpsym  = locatein opchars

   isIdStart '_'  = True
   isIdStart c    = isLower c

   isIdChar c | c == '\''   = True
              | c == '_'    = True
              | otherwise   = isAlphaNum c

   topscan :: String -> IO [ Token ]
   topscan fn = do input  <- readFile fn
                   tokens <- tokenize ('\n':input) (0,1) fn
                   return tokens

   codescan :: String -> IO [ Token ]
   codescan fn = do input  <- readFile fn
                    let (tokens,_,_) = codelines input (1,1) fn
                    return tokens

   -- changed: tokenize_BT replaces part of code in tokenize
   tokenize_BT s ilc fn
     = do let (tname  ,rest ,rlc) = readname s (ilc `advc` 3) fn
              (textlns,orest,olc) = textlines rest rlc fn
          moretoks <- tokenize rest olc fn
          return(    Tok ((TkTextnm ), "",tname, ilc, fn)
                  :  reverse textlns
                  ++ moretoks
                )
   -- changed: tokenize_BC is like tokenize_BT
   tokenize_BC s ilc fn
     = do let (tokens,rest,olc) = codelines (skipline s) (ilc `advl` 1) fn
          moretoks <- tokenize rest olc fn
          return (tokens ++ moretoks)

   tokenize :: String -> Linenumber -> Filename -> IO [ Token ]
   tokenize []       lc  fn = return []
   tokenize ('\n':'\\':'B':'T':s) ilc fn
     = tokenize_BT s ilc fn
   -- new: \begin{HS} is equivalent to \BT
   tokenize ('\n':'\\':'b':'e':'g':'i':'n':'{':'H':'S':'}':s) ilc fn
     = tokenize_BT s ilc fn
   tokenize ('\n':'\\':'B':'C':s) ilc fn
     = tokenize_BC s ilc fn
   -- new: \begin{AG} is equivalent to \BC
   tokenize ('\n':'\\':'b':'e':'g':'i':'n':'{':'A':'G':'}':s) ilc fn
     = tokenize_BC s ilc fn   
   tokenize ('\n':'\\':'I':'N':s) ilc fn
     = do let (fname,rest,olc) = readname s (ilc `advc` 3) fn
          toksfile <- topscan (if null fname then err ilc fn 2 else fname)
          toksrest <- tokenize rest olc fn
          return (toksfile ++ toksrest)
   tokenize ('\n':s) lc  fn = tokenize s (lc `advl` 1) fn
   tokenize (c   :s) lc  fn = tokenize s lc fn

   -- changed: added codelines_EC 
   codelines_EC s stail lc fn
     = if litmode then ([], skipline stail, lc `advl` 1)
                  else codelines (tail s) (lc `advl` 1) fn

   codelines []          lc fn  =  if litmode
                                   then ([Tok (TkError, "","Unterminated code chunk", lc, fn)],[],lc)
                                   else ([],[],lc)
   codelines (c:s)       lc fn  |  isspace c = let
                                                 (sp,next) = span isspace s
                                               in
                                                 codelines
                                                   next
                                                   (lc `advc` (length sp + 1))
                                                   fn
   codelines s@('\n':'\\':'e':'n':'d':'{':'A':'G':'}':stail) lc fn
     = codelines_EC s stail lc fn
   codelines s@('\n':'\\':'E':'C':stail) lc fn
     = codelines_EC s stail lc fn
   codelines ('\n':s)    lc fn  =  codelines s (lc `advl` 1) fn
   codelines ('-':'-':s) lc fn  =  codelines (dropWhile (/= '\n') s) lc fn
   codelines ('{':'-':s) lc fn  =  lexNest codelines s (lc `advc` 2) fn
   codelines ('"':ss)    lc fn
     = let (ys, zs)           = getChars [] ss
           coldiff            = length ss - length zs + 2 -- inefficient!!
           (toks, nrest, nlc) = codelines (tail zs)
                                          (lc `advc` coldiff)
                                          fn
       in  ( (if (head zs /= '"')
               then Tok (TkError , "", "Unterminated string: " ++ reverse ys, lc, fn)
               else Tok (TkString, "", reverse ys                           , lc, fn)
               ) : toks
           , nrest
           , nlc
            )

   codelines ('\'':ss)   lc fn
     = let (ch, rest) = getCh ss
           --validsymbols =  ('\"':specchars) ++ opchars
           validsymbols =  ('\"':' ':specchars) ++ opchars
           getCh (c:rs) | c `elem` validsymbols || isAlphaNum c = (Just3 c, rs)
           getCh ('\\':c:rs)
             | isDigit c || c `elem` ['x','X','o','O'] = getNumberCh (c:rs)
             | otherwise = (getCtrlCh c (Error3 "Invalid esc char"),rs)
           (tok,crest) = if null rest
                         then (Tok (TkError,"","Unterminated char",lc,fn),"")
                         else if (head rest) == '\''
                              then maybe3 (\a -> (Tok (TkChar ,"", [a],lc,fn),tail rest))
                                          (\s -> (Tok (TkError,"", s ,lc,fn),tail rest))
                                          (Tok (TkError,"", "Illegal char definition",lc,fn),tail rest)
                                          ch
                              else (Tok (TkError,"","Unterminated char",lc,fn),rest)
           coldiff            = length ss - length crest + 1 -- inefficient
           (toks, nrest, nlc) = codelines crest (lc `advc` coldiff) fn
       in  ( tok : toks, nrest, nlc )
   codelines ('`':c:s) lc fn
     | isIdStart c || isUpper c
         = let (name', s')        = span isIdChar s
               name               = c:name'
               coldiff            = length s - length crest + 2
               (toks, nrest, nlc) = codelines crest (lc `advc` coldiff) fn
               (tok , crest     )
                 = if null s' || head s' /= '`'
                   then (Tok (TkError, "", "Unterminated infix identifier",lc,fn),s')
                   else if iskw name
                        then (Tok (TkError, ""  , "Keyword used as infix identifier", lc, fn),tail s')
                        else (Tok (TkOp   , name, name , lc, fn),tail s')
           in  (tok:toks, nrest, nlc)
   codelines cs@(c:s) lc fn
     | isSymbol c = let (toks, nrest, nlc) = codelines s (lc `advc` 1) fn
                    in  (Tok (TkSymbol, [c], [c], lc, fn) : toks, nrest, nlc)
     | isIdStart c || isUpper c
         = let (name', s')        = span isIdChar s
               name               = c:name'
               (toks, nrest, nlc) = codelines s' (lc `advc` length name) fn
               tok                = if iskw name
                                    then Tok (TkKeyword     , name, name, lc, fn)
                                    else Tok (if isIdStart c then TkVarid else TkConid
                                                            ,   "", name, lc, fn)
           in (tok:toks, nrest, nlc)
     | isOpsym c = let (name, s') = span isOpsym cs
                       tktype | isop name = TkKeyword
                              | otherwise = TkOp
                       (toks, nrest, nlc) = codelines s'
                                                      (lc `advc` length name)
                                                      fn
                   in  (Tok (tktype, name, name, lc, fn) : toks, nrest, nlc)
     | isDigit c = let (tktype,(number,s')) = getNumber cs
                       coldiff              = length cs - length s'
                       (toks, nrest, nlc)   = codelines s'
                                                        (lc `advc` coldiff)
                                                        fn
                   in  (Tok (tktype, "", number, lc, fn) : toks, nrest, nlc)
     | otherwise = let (toks, nrest, nlc) = codelines s (lc `advc` 1) fn
                   in  ( Tok (TkError, "", "Unexpected character '" ++ [c] ++ "'", lc, fn) : toks
                       , nrest
                       , nlc
                       )

skipline s = let (_,rest) = span (/='\n') s
             in  rest

-- changed: compare with codeslines_EC
textlines_ET s lc fn = ([] ,skipline s,lc)
textlines []                    lc fn = ([Tok (TkError, "", "Unterminated text chunk",lc, fn)],[]        ,lc)
textlines ('\n':'\\':'e':'n':'d':'{':'H':'S':'}':s) lc fn = textlines_ET s lc fn
textlines ('\n':'\\':'E':'T':s) lc fn = textlines_ET s lc fn
textlines ('\n':s) lc fn
  = let (line,rest)     = span (/= '\n') s
        (nls,nrest,nlc) = textlines rest lc fn
    in  (Tok (TkTextln, "", line, lc, fn):nls,nrest,nlc)

-- ks: no clean implementation of columns
readname s lc fn = (name,orest,nlc)
  where (line,irest) = span (/='\n') s
        orest = if null irest then "" else irest
        nlc   = if null irest then lc else (lc `advl` 1)
        name  = takename . dropWhile (\x -> not $ x `elem` "{[") $ line
        takename ln | null ln   = ""
                    | otherwise = if not (null tln) && (isAlpha . head $ tln)
                                  then if not (null rln) && (head rln `elem` "}]")
                                       then cname
                                       else err lc fn 1
                                  else err lc fn 1
          where (cname, rln) = span validChar tln
                tln          = tail ln
                validChar c  = isAlpha c || c `elem` ".-_" || isDigit c

-- ks: changed definition from (lc+1) to (lc)
err lc fn 1 = error ("in scanner bad name definition" ++ maybeshow (lc) fn)
err lc fn 2
   = error ("in scanner not a valid name in file inclusion" ++ maybeshow (lc) fn)

lexNest f ('\n':s)    lc fn = lexNest f s (lc `advl` 1) fn
lexNest f ('-':'}':s) lc fn = f s (lc `advc` 2) fn
lexNest f ('{':'-':s) lc fn = lexNest (lexNest f) s (lc `advc` 2) fn
lexNest f (c:s)       lc fn = lexNest f s (lc `advc` 1) fn
lexNest _ ""          lc fn = ([ Tok (TkError, "", "Unterminated nested comment", lc, fn) ],"",lc)

getChars res [] = (res, [])
getChars res ('\\':c:ss)
  = maybe3 (\a -> getChars (a:res) ss)
           id
           (getChars res  ss)
   . getCtrlCh c $ (Just3 c)
getChars res rest@('"' : ss) = (res, rest)
getChars res rest@('\n': ss) = (res, rest)
getChars res (c: ss)         = getChars (c:res)  ss

getNumber cs@(c:s)
  | c /= '0'               = num10
  | null s                 = const0
  | hs == 'x' || hs == 'X' = num16
  | hs == 'o' || hs == 'O' = num8
  | otherwise              = num10
  where (hs:ts) = s
        const0 = (TkInteger10, ([c],s))
        num10  = (TkInteger10,span isDigit cs)
        num16   = readNum isHexaDigit  ts TkInteger16 ("0",s)
        num8    = readNum isOctalDigit ts TkInteger8  ("0",s)
        readNum p ts tk f
          = let nrs@(n,rs) = span p ts
            in  if null n then (TkInteger10, f  )
                          else (tk         , nrs)

getNumberCh cs@(c:s)
  | c == 'x' || c == 'X' = num16
  | c == 'o' || c == 'O' = num8
  | isDigit c            = num10
  where num10  = readn 10 isDigit cs
        num8   = readn 8  isOctalDigit s
        num16  = readn 16 isHexaDigit  s
        s2n base n = foldr (+) 0
                   . zipWith (\p d -> base ^ p * val d) [0..length n]
                   . reverse $ n
        readn b p s = let nrs@(n,rs) =  span p s
                          cb         = s2n b n
                      in (if (cb >= 0 && cb <= 127) && not (null n)
                          then Just3 . chr $ (if null n then 0 else cb)
                          else if null n
                               then Error3 "Unterminated char"
                               else Error3 "Invalid ASCII code", rs)

isHexaDigit  d = isDigit d || (d >= 'A' && d <= 'F') || (d >= 'a' && d <= 'f')
isOctalDigit d = d >= '0' && d <= '7'

val d | isDigit d = ord d - ord '0'
val d | d >= 'A' && d <= 'F' = 10 + ord d - ord 'A'
val d | d >= 'a' && d <= 'f' = 10 + ord d - ord 'a'

getCtrlCh c o
  | c == 'a'  = Just3 '\a'
  | c == 'b'  = Just3 '\b'
  | c == 'f'  = Just3 '\f'
  | c == 'n'  = Just3 '\n'
  | c == 'r'  = Just3 '\r'
  | c == 't'  = Just3 '\t'
  | c == 'v'  = Just3 '\v'
  | c == '\\' = Just3 '\\'
  | c == '"'  = Just3 '\"'
  | c == '\'' = Just3 '\''
  | c == '&'  = Nothing3
  | otherwise = o

msort []   = []
msort [xs] = xs
msort xss  = msort (mergePairs xss)

mergePairs (xs:ys:xss) = merge xs ys : mergePairs xss
mergePairs xss = xss

merge xxs@(x:xs) yys@(y:ys)
  = if x <= y then
      x:merge xs yys
    else
      y:merge xxs ys
merge [] yys = yys
merge xxs [] = xxs

data Maybe3 a s = Just3    a
                | Error3   s
                | Nothing3

maybe3 fj fe fn m
  = case m of
      Just3 a  -> fj a
      Error3 s -> fe s
      Nothing3 -> fn

-------------------------------------------------------------------------
-- Parsers for  Symbols
-------------------------------------------------------------------------

get_tok_val (Tok( _, _, s, _, _)) = s

gsym :: TokenType -> String -> String -> Parser Token String
gsym kind val val2 = get_tok_val <$> pSym (Tok(kind,val,val2,(0,0),""))

-- ks: added gLsym
--gLsym :: Int -> TokenType -> String -> String -> Parser Token String
--gLsym c kind val val2 = get_tok_val <$> pLSym (Tok(kind,val,val2,(0,0),"")) c

pOper name     =   gsym TkOp        name      name
pKey  keyword  =   gsym TkKeyword   keyword   keyword
pSpec s        =   gsym TkSymbol    [s]       [s]
-- added special case for ;
--pSepc "}"      =   gLsym 10 TkSymbol "}"      "}"
pString        =   gsym TkString    ""        "?STR?"
pChar          =   gsym TkChar      ""        "'chr'"
pInteger8      =   gsym TkInteger8  ""        "1"
pInteger10     =   gsym TkInteger10 ""        "1"
pInteger16     =   gsym TkInteger16 ""        "1"
pVarid         =   gsym TkVarid     ""        "?LC?"
pConid         =   gsym TkConid     ""        "?UC?"
pTextnm        =   gsym TkTextnm    ""        ""
pTextln        =   gsym TkTextln    ""        ""

pInteger       =   pInteger10
{- old
<?> not suppported

pOper_any :: Parser Token String
pOper_any      =      (\(Tok (kind,val, val2,_,_)) -> if kind == TkOp then Yes val else No val, "no operator")
                  <?> pToken (Tok(TkOp, "anyop", "anyop", (0,0), ""))

-}
pComma  = pSpec ','
pSemi   = pSpec ';'
pOParen = pSpec '('
pCParen = pSpec ')'
pOBrack = pSpec '['
pCBrack = pSpec ']'
pOCurly = pSpec '{'
pCCurly = pSpec '}'

pCommas ::  Parser Token a -> Parser Token [a]
pSemics ::  Parser Token a -> Parser Token [a]
pParens ::  Parser Token a -> Parser Token a
pBracks ::  Parser Token a -> Parser Token a
pCurly  ::  Parser Token a -> Parser Token a

pCommas  = pListSep pComma
pSemics  = pListSep pSemi
pParens  = pPacked pOParen pCParen
pBracks  = pPacked pOBrack pCBrack
pCurly   = pPacked pOCurly pCCurly

pParens_pCommas :: Parser Token a -> Parser Token [a]
pBracks_pCommas :: Parser Token a -> Parser Token [a]
pCurly_pSemics  :: Parser Token a -> Parser Token [a]
pParens_pCommas = pParens.pCommas
pBracks_pCommas = pBracks.pCommas
pCurly_pSemics  = pCurly .pSemics



