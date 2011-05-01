{-
	Bogdan Dumitriu
	November 16th, 2004
-}

module CheckDTD where

import Prelude hiding (Just)
import ParseLib

-------------------
-- EBNF van DTD --
-------------------

{-

My grammar is completely based on the original specification that can be found here:

	http://www.w3.org/TR/2000/REC-xml-20001006

Each debatable aspect (specifically, where whitespace _can_ and where whitespace _should_ occur,
what a name can be, what an element can be) is solved by adopting the solution in the document
indicated above. Some things have obviously been taken away from the original, either because
the specification of the assignment didn't require us to cover them or because it was irrelevant
for us (such as various exotic unicode charaters that can appear in a name and so on).

DTD			->  "<!DOCTYPE" Whitespace Name Whitespace? ("[" (ElementDecl | Whitespace)* "]" Whitespace?)? ">"

Name			->  (Letter | "_" | ":") (Letter | Digit | "." | "-" | "_" | ":")*

ElementDecl		->  "<!ELEMENT" Whitespace Name Whitespace ContentSpec Whitespace? ">"

ContentSpec		->  MixedSpec | ChildrenSpec

MixedSpec		->  "(" Whitespace? "#PCDATA" (Whitespace? "|" Whitespace? Name)* Whitespace? ")*"
			|   "(" Whitespace? "#PCDATA" Whitespace? ")"

ChildrenSpec		->  (Choice | Sequence) ("?" | "*" | "+")?

Choice			->  "(" Whitespace? ContentParticle (Whitespace? "|" Whitespace? ContentParticle)+ Whitespace? ")"

Sequence		->  "(" Whitespace? ContentParticle (Whitespace? "," Whitespace? ContentParticle)* Whitespace? ")"

ContentParticle		->  (Name | Choice | Sequence) ("?" | "*" | "+")?

Digit			->  "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

Letter			->  UppercaseLetter | LowercaseLetter

UppercaseLetter		->  "A" | "B" | ... | "Z"

LowercaseLetter		->  "a" | "b" | ... | "z"

Whitespace		->  (" " | "\t" | "\n")+

-}

-------------------------------
-- Abstracte syntax van DTD --
-------------------------------

type DTD	= (Name, [Element])

type Element	= (Name, Content)

data Content	= PCData			-- for mixed content with just #PCDATA, no names, no '*'
		| Mixed    [Name]		-- for any other type of mixed content
		| Children REChildren		-- for content of type children
		deriving (Eq, Ord, Show)

data REChildren	= Just   CContent		-- for ...
		| Option CContent		-- for ...?
		| Many   CContent		-- for ...*
		| Many1  CContent		-- for ...+
		deriving (Eq, Ord, Show)

data CContent	= Choice [REChildren]		-- for n1 | n2 | ...
		| Seq    [REChildren]		-- for n1, n2, ...
		| Name   Name			-- for n1
		deriving (Eq, Ord, Show)

type Name	= String

----------------------
-- Parser voor DTD --
----------------------

--
-- Parser for a DTD.
--
pDTD :: Parser Char DTD
pDTD = (\_ _ name _ elems _ _ -> (name, elems))
	<$> token "<!DOCTYPE"
	<*> pWS
	<*> pName
	<*> pOpWS
	<*> option (pack (symbol '[') pElements (symbol ']')) []
	<*> pOpWS
	<*> symbol '>'

--
-- Parser for a list of elements optionally preceded, separated and/or followed by whitespace.
--
pElements :: Parser Char [Element]
pElements = (\_ elems _ -> elems)
	<$> pOpWS
	<*> listOf pElement pOpWS
	<*> pOpWS

--
-- Parser for an element declaration in a DTD.
--
pElement :: Parser Char Element
pElement = (\_ _ name _ content _ _ -> (name, content))
	<$> token "<!ELEMENT"
	<*> pWS
	<*> pName
	<*> pWS
	<*> pContent
	<*> pOpWS
	<*> symbol '>'

--
-- Parser for any type of element content.
--
pContent :: Parser Char Content
pContent = pMixed
	<|> pChildren

--
-- Parser for mixed content.
--
pMixed :: Parser Char Content
pMixed = pComposed
	<|> pSimple

--
-- Parser for simple type of mixed content.
--
pSimple :: Parser Char Content
pSimple = parenthesised ((\_ _ _ -> PCData)
	<$> pOpWS
	<*> token "#PCDATA"
	<*> pOpWS)

--
-- Parser for the more complicated type of mixed content.
--
pComposed :: Parser Char Content
pComposed = (\_ _ _ names _ _ -> Mixed names)
	<$> symbol '('
	<*> pOpWS
	<*> token "#PCDATA"
	<*> greedy ((\_ _ _ name -> name) <$> pOpWS <*> symbol '|' <*> pOpWS <*> pName)
	<*> pOpWS
	<*> token ")*"

--
-- Parser for content of type children.
--
pChildren :: Parser Char Content
pChildren = (\content res -> Children (constructorFor res content))
	<$> (pChoice <|> pSequence)
	<*> option reSymbol ' '

--
-- Decides on what constructor of the type REChildren to use, depending on what character is
-- received as paramter.
--
constructorFor :: Char -> (CContent -> REChildren)
constructorFor c
  | c == '?'	= Option
  | c == '*'	= Many
  | c == '+'	= Many1
  | otherwise	= Just

--
-- Parser for a '+', '?' or '*'.
--
reSymbol :: Parser Char Char
reSymbol = satisfy (`elem` "?+*")

--
-- Parser for a choice (n1 | n2 | ...)
--
pChoice :: Parser Char CContent
pChoice = parenthesised ((\_ cp cps _ -> Choice (cp:cps))
	<$> pOpWS
	<*> pContentParticle
	<*> greedy1 ((\_ _ _ cp -> cp) <$> pOpWS <*> symbol '|' <*> pOpWS <*> pContentParticle)
	<*> pOpWS)

--
-- Parser for a sequence (n1, n2, ...)
--
pSequence :: Parser Char CContent
pSequence = parenthesised ((\_ cp cps _ -> Seq (cp:cps))
	<$> pOpWS
	<*> pContentParticle
	<*> greedy ((\_ _ _ cp -> cp) <$> pOpWS <*> symbol ',' <*> pOpWS <*> pContentParticle)
	<*> pOpWS)

--
-- Parser for a content particle.
--
pContentParticle :: Parser Char REChildren
pContentParticle = (\content res -> constructorFor res content)
	<$> (Name <$> pName <|> pChoice <|> pSequence)
	<*> option reSymbol ' '

--
-- Parser for a name.
--
pName :: Parser Char Name
pName = (:)
	<$> (pLetter <|> satisfy (`elem` "_:"))
	<*> greedy (pLetter <|> pDigit <|> satisfy (`elem` ".-_:"))

--
-- Parser for a letter.
--
pLetter :: Parser Char Char
pLetter = satisfy (`elem` (['a' .. 'z'] ++ ['A' .. 'Z']))

--
-- Parser for a digit.
--
pDigit :: Parser Char Char
pDigit = satisfy (`elem` ['0' .. '9'])

--
-- Parser for optional whitespace.
--
pOpWS :: Parser Char String
pOpWS = greedy (satisfy isWhitespace)

--
-- Parser for whitespace.
--
pWS :: Parser Char String
pWS = greedy1 (satisfy isWhitespace)

--
-- Returns true if character is space, tab or new line.
--
isWhitespace :: Char -> Bool
isWhitespace = (`elem` " \t\n")

----------------------------
-- Fold voor DTD --
----------------------------

type DTDAlgebra d e c r cc = (Name -> [e] -> d,
			      Name -> c -> e,
			      (c, [Name] -> c, r -> c),
			      (cc -> r, cc -> r, cc -> r, cc -> r),
			      ([r] -> cc, [r] -> cc, Name -> cc))

foldDTD :: DTDAlgebra d e c r cc -> DTD -> d
foldDTD (fDtd, fElement, (fPcdata, fMixed, fChildren), (fJust, fOption, fMany, fMany1), (fChoice, fSeq, fName)) = fold
  where
  fold (n, e)		= fDtd n (map foldE e)
  foldE (n, c)		= fElement n (foldC c)
  foldC PCData		= fPcdata
  foldC (Mixed n)	= fMixed n
  foldC (Children rec)	= fChildren (foldR rec)
  foldR (Just cc)	= fJust (foldCC cc)
  foldR (Option cc)	= fOption (foldCC cc)
  foldR (Many cc)	= fMany (foldCC cc)
  foldR (Many1 cc)	= fMany1 (foldCC cc)
  foldCC (Choice rec)	= fChoice (map foldR rec)
  foldCC (Seq rec)	= fSeq (map foldR rec)
  foldCC (Name n)	= fName n

--------------------------
-- Pretty-printing DTD --
--------------------------

ppAlg :: DTDAlgebra String String String String String
ppAlg = (fDtd, fElement, (fPcdata, fMixed, fChildren), (fJust, fOption, fMany, fMany1), (fChoice, fSeq, fName))
  where
  fDtd n e	= "<!DOCTYPE " ++ n ++ "\n[\n" ++ concat e ++ "]>\n"
  fElement n c	= "<!ELEMENT " ++ n ++ " " ++ c ++ ">\n"
  fPcdata	= "(#PCDATA)"
  fMixed n	= "(#PCDATA" ++ concat (map ('|':) n) ++ ")*"
  fChildren r	= r
  fJust	cc	= cc
  fOption cc	= cc ++ "?"
  fMany cc	= cc ++ "*"
  fMany1 cc	= cc ++ "+"
  fChoice r	= "(" ++ separateWith "|" r ++ ")"
  fSeq r	= "(" ++ separateWith "," r ++ ")"
  fName n	= n

ppDTD :: DTD -> String
ppDTD = foldDTD ppAlg

printDTD :: DTD -> IO ()
printDTD = putStr . ppDTD

separateWith :: [a] -> [[a]] -> [a]
separateWith c []	= []
separateWith c [x]	= x
separateWith c (x:xs)	= x ++ c ++ separateWith c xs

--------------------------
-- checking a DTD --
--------------------------

type Env = [Name]

checkDTD :: DTD -> Bool
checkDTD dtd = result
  where
  (env, result) = foldDTD checkAlgebra dtd env

checkAlgebra :: DTDAlgebra (Env -> (Env, Bool)) (Env -> (Name, Bool)) (Env -> Bool) (Env -> Bool) (Env -> Bool)
checkAlgebra = (fDtd, fElement, (fPcdata, fMixed, fChildren), (fJust, fOption, fMany, fMany1), (fChoice, fSeq, fName))
  where
  fDtd n e env		= (names, and retvals)
    where
    (names, retvals)	= unzip (map ($ env) e)
  fElement n c env	= (n, c env)
  fPcdata env		= True
  fMixed n env		= and (map (`elem` env) n)
  fChildren r env	= r env
  fJust	cc env		= cc env
  fOption cc env	= cc env
  fMany cc env		= cc env
  fMany1 cc env		= cc env
  fChoice r env		= and (map ($ env) r)
  fSeq r env		= and (map ($ env) r)
  fName n env		= elem n env

--------------------
-- Test functions --
--------------------
 
bookDTD = "<!DOCTYPE BookDTD [" ++ bookelements ++ "]>"
bookelements = bookelement1 ++ bookelement2 ++ bookelement3 
bookelement1 = "\n<!ELEMENT book (introduction,chapter*)>"
bookelement2 = "\n<!ELEMENT introduction (prologue|preface)>"
bookelement3 = "\n<!ELEMENT chapter (#PCDATA)>"
 
bookDTDcomplete  = "<!DOCTYPE BookDTD [" ++ bookelementscomplete ++ "]>"
bookelementscomplete = bookelement1 ++ bookelement2 ++ bookelement3 ++ bookelement4 ++ bookelement5
bookelement4 = "\n<!ELEMENT prologue (#PCDATA)>"
bookelement5 = "\n<!ELEMENT preface (#PCDATA)>"
 
recipeDTD = "<!DOCTYPE RecipeDTD [" ++ recipeElement1 ++ recipeElement2 ++ recipeElement3 ++ recipeElement4 ++ "]>"
recipeElement1 = "\n<!ELEMENT recipe (title,ingredients,preparation)>"
recipeElement2 = "\n<!ELEMENT title (#PCDATA)>"
recipeElement3 = "\n<!ELEMENT ingredients (ingredient)*>"
recipeElement4 = "\n<!ELEMENT preparation (ingredients,action*,ingredients)>"

testBookDTD		= fst (head (pDTD bookDTD))
testBookDTDComplete	= fst (head (pDTD bookDTDcomplete))
testRecipeDTD		= fst (head (pDTD recipeDTD))