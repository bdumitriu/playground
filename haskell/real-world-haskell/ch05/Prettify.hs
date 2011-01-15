module Prettify (
  Doc
, char
, text
, double
, line
, (<>)
, hcat
, fsep
, punctuate
, compact
, pretty
, indent
) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Eq, Show)

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = Text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = x `Concat` y

hcat :: [Doc] -> Doc
hcat = foldr (<>) Empty

-- Separates all the Doc values in the list with the punctuation Doc.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

-- Combines a list of Doc values into one, possibly wrapping lines.
fsep :: [Doc] -> Doc
fsep = foldr (</>) Empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

-- Produces a Union of a Line and a space (Char ' ').
softline :: Doc
softline = group line

-- Groups (flatten x) and x into a Union.
group :: Doc -> Doc
group x = flatten x `Union` x

-- Turns all lines of a document into spaces.
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

-- Renders a document in a compact style.
compact :: Doc -> String
compact x = transform [x]
  where transform []     = ""
        transform (d:ds) = case d of
          Empty        -> transform ds
          Char c       -> c : transform ds
          Text s       -> s ++ transform ds
          Line         -> '\n' : transform ds
          a `Concat` b -> transform (a:b:ds)
          _ `Union` b  -> transform (b:ds)

-- Renders a document in a pretty style.
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) = case d of
          Empty        -> best col ds
          Char c       -> c : best (col + 1) ds
          Text s       -> s ++ best (col + length s) ds
          Line         -> '\n' : best 0 ds
          a `Concat` b -> best col (a:b:ds)
          a `Union` b  -> nicest col (best col (a:ds))
                                     (best col (b:ds))
        best _ _        = ""

        nicest col a b
          | (width - least) `fits` a = a
          | otherwise                = b
          where least = min width col

-- Decides whether a string can fit in a given number of columns. The function
-- also stops if a new line character is found.
fits :: Int -> String -> Bool
fits w _
  | w < 0       = False
fits w ""       = True
fits w ('\n':_) = True
fits w (c:cs)   = (w-1) `fits` cs

-- renders a document in an indented style
-- (The solution is a bit crappy because it indents the closing '}'/']' as well.)
indent :: Int -> Doc -> Doc
indent indentation = fst . nest 0
  where nest nestingLevel Empty          = (Empty, nestingLevel)
        nest nestingLevel doc@(Char c)
          | c == '{' || c == '['                = (doc, nestingLevel + 1)
          | c == '}' || c == ']'                = (doc, max 0 (nestingLevel - 1))
          | otherwise                           = (doc, nestingLevel)
        nest nestingLevel doc@(Text s)   = (doc, nestingLevel)
        nest nestingLevel Line           = (Line `Concat` (text (replicate (nestingLevel * indentation) ' ')), nestingLevel)
        nest nestingLevel (a `Concat` b) = let (aDoc, aNestingLevel) = nest nestingLevel a
                                               (bDoc, bNestingLevel) = nest aNestingLevel b
                                           in (aDoc `Concat` bDoc, bNestingLevel)
        nest nestingLevel (a `Union` b)  = ((fst . nest nestingLevel $ a) `Union` (fst . nest nestingLevel $ b), nestingLevel)
