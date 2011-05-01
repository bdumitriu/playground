module PropParser(parseProp, xparseProp)

where

import PropDD
import ResultDD
import Parsec
import qualified ParsecToken as P
import ParsecExpr
import List



lexer :: P.TokenParser ()
lexer  
	= 
	P.makeTokenParser lexerDef

lexerDef =
	P.LanguageDef
	{ P.commentStart    = "{-"
    	, P.commentEnd      = "-}"
    	, P.commentLine     = "--"
    	, P.nestedComments  = False
    	, P.identStart      = letter
    	, P.identLetter     = alphaNum <|> oneOf "_'"
    	, P.opStart         = oneOf ""
    	, P.opLetter        = (oneOf . nub . concat . P.reservedOpNames) lexerDef
    	, P.reservedNames   = ["true","false"]
    	, P.reservedOpNames = ["+","-","*","/","<","<=",">",">=","=",
                               "==>","/\\","\\/","~", "<=>"]
    	, P.caseSensitive   = True
    	}                           
	
whiteSpace = P.whiteSpace lexer
--lexeme     = P.lexeme lexer
--symbol     = P.symbol lexer
integer    = P.integer lexer
parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
		
         
pExpr    :: Parser Prop
pExpr    = buildExpressionParser opTable pFactor
           <?> 
	   "expression"

opTable  = 
	[[unary_op "~" mk_NOT],

	 [binop "/\\" (mk_Binary "AND") AssocLeft],
 
	 [binop "\\/" (mk_Binary "OR") AssocLeft],

	 [binop "==>" (mk_Binary "IMP") AssocRight],

	 [binop "<=>" (mk_Binary "EQUIV") AssocNone]
	]
 
        where

          binop s f assoc
             = Infix (do{reservedOp s; return f}) assoc

	  unary_op s f 
	     = Prefix (do{reservedOp s; return f})	


pFactor  =  parens pExpr
	<|> pBool
	<|> pIdent
        <?> "simple expression"

pBool :: Parser Prop
pBool 
	= 
	do { reserved "true" ; return mk_TT		}
	<|>
	do { reserved "false" ; return mk_FF		}

pIdent :: Parser Prop
pIdent 
	= 
	do { i <- identifier					;
	     return (mk_Ident i)				}


either2Result (Right ok) = Success ok
either2Result (Left error)  = Fail error

parseProp input = either2Result (parse pExpr "" (dropWhile (==' ') input))

xparseProp :: String->Prop
xparseProp input = case parseProp input of
	Success r -> r