{-
	Bogdan Dumitriu
	October 29rd, 2004
-}

module Recipes where

import ParseLib
import Calories
import Char
import Numeric hiding (showFloat)

--
-- Assignment 2: Recipes
--

--------------------------------------------------------------------------------
-- Concrete (EBNF) grammar
--------------------------------------------------------------------------------

{-

Recipe         ->  Title "\n\n" Ingredients "\n\n" Preparation
Title          ->  String
Ingredients    ->  "Ingredients" ("\n" Ingredient)*
Ingredient     ->  Quantity? Product

Quantity       ->  Mass | Volume | Number
Mass           ->  FixedPointNr MassUnit
Volume         ->  FixedPointNr VolumeUnit
Number         ->  FixedPointNr NumberUnit?
MassUnit       ->  K G | G | M G | O N S
VolumeUnit     ->  L | D L | M L
NumberUnit     ->  Spoonful | TeaSpoonful | Tin | Bag | Slice | Block | Bottle | Piece | ...
FixedPointNr   ->  Digits0 ("." Digit+)?
Digits0        ->  "0" | Digit0 Digit*
Digit          ->  "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
Digit0         ->  "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
Spoonful       ->  S P O O N F U L S?
TeaSpoonful    ->  T E A S P O O N F U L S?
Tin            ->  T I N S?
Bag            ->  B A G S?
Slice          ->  S L I C E S?
Block          ->  B L O C K S?
Bottle         ->  B O T T L E S?
Piece          ->  P I E C E S?

Product        ->  Word (Whitespace Word)*
Word           ->  Char+
Whitespace     ->  (" " | "\t")+

Preparation    ->  "Preparation\n" TimeFrame*
TimeFrame      ->  Digits0 "min." Action+
Action         ->  IngredientList "->" Instructions "->" IngredientList "\n"
IngredientList ->  Ingredient ("," Ingredient)*

Char           ->  "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"

A              ->  "a" | "A"
B              ->  "b" | "B"
C              ->  "c" | "C"
...
Z              ->  "z" | "Z"

-}

--
-- Abstract syntax
--

type Recipe		= (Title, [Ingredient], Preparation)

type Title		= String

type Ingredient		= ([Quantity], Product)
  -- [Quantity] represents one quantity or no quantity at all.
  -- Optionality is usually represented using the Maybe type in Haskell,
  -- but that is not covered by the lecture.

type Product		= String

type Word		= String

--
-- I've created a new class, Show_, so that I would still be able to use the
-- standard show method for all my data types for testing purposes.
--
class Show_ a where
  show_ :: a -> String

data Quantity		= Mass (Float, MassUnit)
			| Volume (Float, VolumeUnit)
			| Number (Float, [NumberUnit])
			deriving (Eq, Ord, Show)

instance Show_ Quantity where
  show_ (Mass (fValue, mu))	= showFloat fValue ++ ' ' : show_ mu
  show_ (Volume (fValue, vu))	= showFloat fValue ++ ' ' : show_ vu
  show_ (Number (fValue, []))	= showFloat fValue
  show_ (Number (fValue, [nu]))	= showFloat fValue ++ ' ' : show_ nu ++ decidePlural fValue
    where
    decidePlural fValue
      | fValue /= 1.0	= "s"
      | otherwise	= ""

--
-- Shows a float normally if the part after the "." is different from 0 and just shows
-- the integer part of the float if the part after the "." is 0.
--
showFloat :: Float -> String
showFloat x
  | last sx == '0'	= take (length sx - 2) sx
  | otherwise		= sx
  where
  sx = showFFloat Nothing x ""

data MassUnit		= Kilogram | Gram | Miligram | Ons
			deriving (Eq, Ord, Show)

instance Show_ MassUnit where
  show_ Kilogram	= "kg"
  show_ Gram		= "g"
  show_ Miligram	= "mg"
  show_ Ons		= "ons"

data VolumeUnit		= Liter | Deciliter | Mililiter
			deriving (Eq, Ord, Show)

instance Show_ VolumeUnit where
  show_ Liter		= "l"
  show_ Deciliter	= "dl"
  show_ Mililiter	= "ml"

data NumberUnit		= Spoonful | TeaSpoonful | Tin | Bag | Slice | Block | Bottle | Piece
			deriving (Eq, Ord, Show)

instance Show_ NumberUnit where
  show_ Spoonful	= "spoonful"
  show_ TeaSpoonful	= "teaspoonful"
  show_ Tin		= "tin"
  show_ Bag		= "bag"
  show_ Slice		= "slice"
  show_ Block		= "block"
  show_ Bottle		= "bottle"
  show_ Piece		= "piece"

type Preparation	= [TimeFrame]

type TimeFrame		= (Time, [Action])

type Time		= Int

type Action		= ([Ingredient], Instructions ,[Ingredient])

type Instructions	= String

--
-- Unparser
--

separateWith :: [a] -> [[a]] -> [a]
separateWith c []	= []
separateWith c [x]	= x
separateWith c (x:xs)	= x ++ c ++ separateWith c xs

unparseRecipe :: Recipe -> String
unparseRecipe (title, ingredients, preparation) = title
	++ "\n\n"
	++ "Ingredients\n"
	++ separateWith "\n" (map unparseIngredient ingredients)
	-- the modification below was necessary in order to ensure that only two '\n's would be printed even
	-- if the lists of ingredients is empty
	++ (if not (null ingredients) then "\n\n" else "\n")
	++ "Preparation\n"
	++ unparsePreparation preparation
	-- the modification below was necessary in order to ensure that only two '\n's would be printed even
	-- if the lists of preparation actions is empty
	++ (if not (null preparation) then "\n\n" else "\n")
    
unparseIngredient :: Ingredient -> String
unparseIngredient ([], product) = product
unparseIngredient ([q], product) = show_ q ++ ' ' : product

unparsePreparation :: Preparation -> String
unparsePreparation = separateWith "\n" . map unparseTimeFrame

unparseTimeFrame :: TimeFrame -> String
unparseTimeFrame (time, actions) = show time
	++ " min. "
	++ separateWith "\n" (map unparseAction actions)

unparseAction :: Action -> String
unparseAction (inputIngredients, instructions, outputIngredients) =
	separateWith ", " (map unparseIngredient inputIngredients)
	++ " -> "
	++ instructions
	++ " -> "
	++ separateWith ", " (map unparseIngredient outputIngredients)

--
-- Parser
--

pRecipe :: Parser Char Recipe
pRecipe = (\x _ _ y _ _ z -> (x, y, z)) <$> pTitle <*> newline <*> newline <*> pIngredients <*> newline <*> newline <*> pPreparation

parseRecipe :: String -> Recipe
parseRecipe st
  | null recipes	= error "parse error"
  | otherwise		= fst (head recipes)
  where
  recipes = pRecipe st

{-
	Assignment 6: Is parseRecipe . unparseRecipe :: Recipe -> Recipe the identity function for values of type Recipe?
	
	Yes, it is. The reason is that unparseRecipe will generate a representation which (if both functions are correctly
	implemented) should be parsable back to the original Recipe. After all, these were the requirements for the unparseRecipe
	and parseRecipe functions.
-}

--
-- Parser for the title of the recipe.
--
pTitle :: Parser Char Title
pTitle = greedy (satisfy (/= '\n'))

--
-- Parser for a list of ingredients, each on its own line, preceeded by the word
-- "Ingredients", also on a line of its own.
--
pIngredients :: Parser Char [Ingredient]
pIngredients = curry snd <$> token "Ingredients" <*> option (curry snd <$> newline <*> listOf pIngredient newline) []

--
-- Parser for an ingredient.
--
pIngredient :: Parser Char Ingredient
pIngredient = (\_ x y -> (x, y)) <$> option whitespace "" <*> option (curry fst <$> pQuantity <*> whitespace) [] <*> pProduct

--
-- Parser for a quantity.
--
pQuantity :: Parser Char [Quantity]
pQuantity = pMass
	<|> pVolume
	<|> pNumber

--
-- Parser for a mass quantity.
--
pMass :: Parser Char [Quantity]
pMass = (\x _ y -> [Mass (x, y)]) <$> pFixedPointNr <*> whitespace <*> pMassUnit

--
-- Parser for units which can appear next to masses.
--
pMassUnit :: Parser Char MassUnit
pMassUnit = const Kilogram <$> tokenCI "kg"
	<|> const Gram <$> tokenCI "g"
	<|> const Miligram <$> tokenCI "mg"
	<|> const Ons <$> tokenCI "ons"

--
-- Parser for a volume quantity.
--
pVolume :: Parser Char [Quantity]
pVolume = (\x _ y -> [Volume (x, y)]) <$> pFixedPointNr <*> whitespace <*> pVolumeUnit

--
-- Parser for units which can appear next to volumes.
--
pVolumeUnit :: Parser Char VolumeUnit
pVolumeUnit = const Liter <$> tokenCI "l"
	<|> const Deciliter <$> tokenCI "dl"
	<|> const Mililiter <$> tokenCI "ml"

--
-- Parser for a simple number quantity.
--
pNumber :: Parser Char [Quantity]
pNumber = (\x y -> [Number (x, y)]) <$> pFixedPointNr <*> option ((\_ x -> [x]) <$> whitespace <*> pNumberUnit) []

--
-- Parser for units which can appear next to simple numbers.
--
pNumberUnit :: Parser Char NumberUnit
pNumberUnit = const . const Spoonful <$> tokenCI "spoonful" <*> option (symbolCI 's') ' '
	<|> const . const TeaSpoonful <$> tokenCI "teaspoonful" <*> option (symbolCI 's') ' '
	<|> const . const Tin <$> tokenCI "tin" <*> option (symbolCI 's') ' '
	<|> const . const Bag <$> tokenCI "bag" <*> option (symbolCI 's') ' '
	<|> const . const Slice <$> tokenCI "slice" <*> option (symbolCI 's') ' '
	<|> const . const Block <$> tokenCI "block" <*> option (symbolCI 's') ' '
	<|> const . const Bottle <$> tokenCI "bottle" <*> option (symbolCI 's') ' '
	<|> const . const Piece <$> tokenCI "piece" <*> option (symbolCI 's') ' '

--
-- Parser for a String token, but which is case insensitive.
--
tokenCI :: String -> Parser Char String
tokenCI = psequence . map symbolCI

--
-- Parser for a Char symbol, but which is case insensitive.
--
symbolCI :: Char -> Parser Char Char
symbolCI ch
  | 'a' <= ch && ch <= 'z'	= symbol ch <|> symbol (chr (ord ch - ord 'a' + ord 'A'))
  | 'A' <= ch && ch <= 'Z'	= symbol ch <|> symbol (chr (ord ch - ord 'A' + ord 'a'))
  | otherwise			= symbol ch

--
-- Parser for a fixed point number which returns a Float.
--
pFixedPointNr :: Parser Char Float
pFixedPointNr = read <$> pFixedPointNrString

--
-- Parser for a fixed point number which returns a String.
--
pFixedPointNrString :: Parser Char String
pFixedPointNrString = (++) <$> pDigits0 <*> option (list <$> symbol '.' <*> pDigits) ""

--
-- Parser for "0" or for a sequence of digits which doesn't begin with a '0'.
--
pDigits0 :: Parser Char String
pDigits0 = token "0"
	<|> list <$> pDigit0 <*> greedy pDigit

--
-- Parser for any non-empty sequence of digits.
--
pDigits :: Parser Char String
pDigits = greedy1 pDigit

--
-- Parser for any digit other than '0'.
--
pDigit0 :: Parser Char Char
pDigit0 = satisfy (\x -> isDigit x && x /= '0')

--
-- Parser for any digit.
--
pDigit :: Parser Char Char
pDigit = satisfy isDigit

--
-- Parser for a product name.
--
pProduct :: Parser Char Product
pProduct = pWords

--
-- Parser for a sequence of words separated by whitespace, which requires at least one
-- word to be present. The result also includes the whitespace, as it appears in the string.
--
pWords :: Parser Char String
pWords = (\x y -> x ++ concat y) <$> pWord <*> many ((++) <$> whitespace <*> pWord)

--
-- Parser for a word.
--
pWord :: Parser Char Word
pWord = listOf (satisfy isAlpha) (succeed "")

--
-- Parser for the preparation part of the recipe.
--
pPreparation :: Parser Char Preparation
pPreparation = (\_ _ x _ -> x) <$> token "Preparation" <*> newline <*> option (curry fst <$> listOf pTimeFrame newline <*> newline) [] <*> newline

--
-- Parser for a time frame of the preparation.
--
pTimeFrame :: Parser Char TimeFrame
pTimeFrame = (\x _ _ y -> (read x, y)) <$> pDigits0 <*> whitespace <*> token "min." <*> pActions

--
-- Parser for a list of preparation actions.
--
pActions :: Parser Char [Action]
pActions = listOf pAction newline

--
-- Parser for a preparation action.
--
pAction :: Parser Char Action
pAction = (\_ x _ _ _ y _ _ _ z -> (x, y, z)) <$>
	option whitespace "" <*>
	pIngredientList <*>
	whitespace <*>
	token "->" <*>
	whitespace <*>
	pInstructions <*>
	whitespace <*>
	token "->" <*>
	whitespace <*>
	pIngredientList

--
-- Parser for a list of ingredients separated by commas.
--
pIngredientList :: Parser Char [Ingredient]
pIngredientList = option (listOf pIngredient ((\_ _ _ -> "") <$> option whitespace "" <*> symbol ',' <*> option whitespace "")) []

--
-- Parser for the instructions of an action.
--
pInstructions :: Parser Char Instructions
pInstructions = pWords

--
-- Parser for a sequence consisting of one or more spaces and/or tabs.
--
whitespace :: Parser Char [Char]
whitespace = greedy1 (symbol ' ' <|> symbol '\t')

--
-- Parser for a new line symbol.
--
newline :: Parser Char Char
newline = symbol '\n'

--
-- Normalize
--

normalizeRecipe :: Recipe -> Recipe
normalizeRecipe (title, ingredients, preparation) = (title, normalizeIngredients ingredients, normalizePreparation preparation)

normalizeIngredients :: [Ingredient] -> [Ingredient]
normalizeIngredients = map normalizeIngredient

normalizeIngredient :: Ingredient -> Ingredient
normalizeIngredient ([], product)		= ([], product)
normalizeIngredient ([quantity], product)	= ([normalizeQuantity quantity], product)

normalizeQuantity :: Quantity -> Quantity
normalizeQuantity (Mass x)	= normalizeMass (Mass x)
normalizeQuantity (Volume x)	= normalizeVolume (Volume x)
normalizeQuantity x		= x

normalizeMass :: Quantity -> Quantity
normalizeMass (Mass (val, Kilogram))	= Mass (val * 10, Ons)
normalizeMass (Mass (val, Gram))	= Mass (val / 100, Ons)
normalizeMass (Mass (val, Miligram))	= Mass (val / 100000, Ons)
normalizeMass x				= x

normalizeVolume :: Quantity -> Quantity
normalizeVolume (Volume (val, Liter))		= Volume (val * 10, Deciliter)
normalizeVolume (Volume (val, Mililiter))	= Volume (val / 100, Deciliter)
normalizeVolume x				= x

normalizePreparation :: Preparation -> Preparation
normalizePreparation = map normalizeTimeFrame

normalizeTimeFrame :: TimeFrame -> TimeFrame
normalizeTimeFrame (time, actions) = (time, normalizeActions actions)

normalizeActions :: [Action] -> [Action]
normalizeActions = map normalizeAction

normalizeAction :: Action -> Action
normalizeAction (inputIngredients, instructions, outputIngredients) = (normalizeIngredients inputIngredients, instructions, normalizeIngredients outputIngredients)

--
-- Calories
--

--
-- Returns the calories for 1 person, i.e. computes the total calories and
-- divedes them by 2.
--
caloriesRecipe :: Recipe -> KCal
caloriesRecipe (_, ingredients, _) = caloriesIngredients (normalizeIngredients ingredients) / 2

caloriesIngredients :: [Ingredient] -> KCal
caloriesIngredients = sum . map caloriesIngredient

caloriesIngredient :: Ingredient -> KCal
caloriesIngredient ([], product)		= lookupKCal product
caloriesIngredient ([Mass (val, _)], product)	= (lookupKCal product) * val
caloriesIngredient ([Volume (val, _)], product)	= (lookupKCal product) * val
caloriesIngredient ([Number (val, _)], product)	= (lookupKCal product) * val

lookupKCal :: Product -> KCal
lookupKCal product = maybe (error ("energetic value for " ++ product ++ " is not defined")) id (lookup product kcalTable)

--
-- Preparation time
--
timeRecipe :: Recipe -> Time
timeRecipe (_, _, preparation) = sum (map fst preparation)

--
-- Shopping list
--

--
-- Assume that each recipe is intended for two persons.
--
shoppinglist :: Int -> Recipe -> [Ingredient]
shoppinglist n (_, ingredients, _) = shoppinglistIngredients (fromIntegral n / 2) ingredients

shoppinglistIngredients :: Float -> [Ingredient] -> [Ingredient]
shoppinglistIngredients n ingredients = map f ingredients
  where
  f ([], product)			= ([], product)
  f ([(Mass (val, mu))], product)	= ([(Mass (val*n, mu))], product)
  f ([(Volume (val, vu))], product)	= ([(Volume (val*n, vu))], product)
  f ([(Number (val, nu))], product)	= ([(Number (val*n, nu))], product)
  
--
-- Example inputs
--

-- Example 1 Bossche Bollen

bosschebollen =  bbtitle
	++ "\n"
	++ bbingredients
	++ "\n"
	++ bbbereiding
	++ "\n"

bbtitle = "Bossche bollen\n"

bbingredients = "Ingredients\n"
	++ bbing1
	++ "\n"
	++ bbing2
	++ "\n"
	++ bbing3
	++ "\n"
	++ bbing4
	++ "\n"
	++ bbing5
	++ "\n"
	++ bbing6
	++ "\n"
	++ bbing7
	++ "\n"
	++ bbing8
	++ "\n"

bbing1 = "500 g        flour"
bbing2 = "1 ons        butter"
bbing3 = "3            eggs"
bbing4 = "1.5 dl       water"
bbing5 = "2.5 dl       whipped cream"
bbing6 = "2 spoonfuls  cocoa"
bbing7 = "125 g        powdered sugar"
bbing8 = "             salt"

bbbereiding = "Preparation\n"
	++ bbtime1
	++ bbaction1
	++ "\n"
	++ bbaction2
	++ "\n"
	++ bbaction3
	++ "\n"
	++ bbtime2
	++ bbaction4
	++ "\n"
	++ bbaction5
	++ "\n"
	++ bbaction6
	++ "\n"

bbtime1 = "5 min.  "
bbtime2 = "20 min. "

bbaction1 = "flour, butter, 1 dl water -> mix -> pastry"
bbaction2 = "        pastry, eggs -> add eggs one by one -> pastry"
bbaction3 = "        pastry -> divide into small towers -> towers"
bbaction4 = "towers -> bake until brown -> towers"
bbaction5 = "        cocoa, 0.5 dl water, powderes sugar -> mix -> cocoa mix"
bbaction6 = "        towers, cocoa mix -> coat towers with cocoa mix -> bossche bollen"

-- Example 2 Pasta Bolognese

pastabolognese = pbtitle
	++ "\n"
	++ pbingredients
	++ "\n"
	++ pbpreparation
	++ "\n"

pbtitle = "Pasta Bolognese\n"

pbingredients = "Ingredients\n"
	++ pbing1
	++ "\n"
	++ pbing2
	++ "\n"
	++ pbing3
	++ "\n"
	++ pbing4
	++ "\n"

pbing1 = "100 g    fresh pasta"
pbing2 = "1 tin    tomato blocks"
pbing3 = "1 bag    parmesan cheese"
pbing4 = "1        onion"

pbpreparation = "Preparation\n"
	++ pbtime1
	++ pbaction1
	++ "\n"
	++ pbtime2
	++ pbaction2
	++ "\n"
	++ pbaction3
	++ "\n"
	++ pbaction4
	++ "\n"

pbtime1 = "2 min.  "
pbtime2 = "10 min. "
pbaction1 = "onion -> cut onion into rings -> onion rings"
pbaction2 = "pasta -> cook -> pasta"
pbaction3 = "        onion rings and tomato blocks -> fry -> onion tomato mix"
pbaction4 = "        pasta, onion tomato mix -> mix -> pasta bolognese"

-- Example 3 Fish soep

fishsoup = fstitle
	++ "\n"
	++ fsingredients
	++ "\n"
	++ fspreparation
	++ "\n"

fstitle = "Fish soup\n"

fsingredients = "Ingredients\n"
	++ fsing1
	++ "\n"
	++ fsing2
	++ "\n"
	++ fsing3
	++ "\n"
	++ fsing4
	++ "\n"
	++ fsing5
	++ "\n"
	++ fsing6
	++ "\n"

fsing1 = "2 slices   bread"
fsing2 = "1 block    fish bouillon"
fsing3 = "300 g      codfish"
fsing4 = "100 g      prawns"
fsing5 = "           salt"
fsing6 = "1 ml        water"
 
fspreparation = "Preparation\n"
	++ fstime1
	++ fsaction1
	++ "\n"
	++ fsaction2
	++ "\n"
	++ fsaction3
	++ "\n"
	++ fstime2
	++ fsaction4
	++ "\n"

fstime1 = "30 min. "
fstime2 = "5 min.  "
fsaction1 = "codfish -> cook -> codfish"
fsaction2 = "        2 slices bread -> crumble -> breadcrumbs"
fsaction3 = "        water, fish bouillon -> dissolve fish boullon in boiling water -> soup"
fsaction4 = "codfish, breadcrumbs, soup, prawns, salt -> mix -> fish soup"

--
-- Parsings of examples
-- 

bosschebollenRecipe :: Recipe
bosschebollenRecipe = ("Bossche bollen",
	[
		([Mass (500.0, Gram)], "flour"),
		([Mass (100.0, Gram)], "butter"),
		([Number (3.0, [])], "eggs"),
		([Volume (1.5, Deciliter)], "water"),
		([Volume (2.5, Deciliter)], "whipped cream"),
		([Number (2.0, [Spoonful])], "cocoa"),
		([Mass (125.0, Gram)], "powdered sugar"),
		([], "salt")
	],
	[
		(5,
		[
			([([], "flour"), ([], "butter"), ([Volume (1.0, Deciliter)], "water")], "mix", [([], "pastry")]),
			([([], "pastry"), ([], "eggs")], "add eggs one by one", [([], "pastry")]),
			([([], "pastry")], "divide into small towers", [([], "towers")])
		]),
		(20,
		[
			([([], "towers")], "bake until brown", [([], "towers")]),
			([([], "cocoa"), ([Volume (0.5, Deciliter)], "water"), ([], "powdered sugar")], "mix", [([], "cocoamix")]),
			([([], "towers"), ([], "cocoamix")],"coat towers with cocoa mix", [([], "bossche bollen")])
		])
	])

fishsoupRecipe :: Recipe
fishsoupRecipe = ("Fish soup",
	[
		([Number (2.0, [Slice])], "bread"),
		([Number (1.0, [Block])], "fish bouillon"),
		([Mass (300.0, Gram)], "codfish"),
		([Mass (100.0, Gram)], "prawns"),
		([], "salt"),
		([Volume (1.0, Liter)], "water")
	],
	[
		(30,
		[
			([([], "codfish")], "cook", [([], "codfish")]),
			([([Number (2.0, [Slice])], "bread")], "crumble", [([], "breadcrumbs")]),
			([([], "water"), ([], "fish bouillon")], "dissolve fish bouillon in boiling water", [([], "soup")])
		]),
		(5,
		[
			([([], "codfish"), ([], "breadcrumbs"), ([], "soup"), ([], "prawns"), ([], "salt")], "mix", [([], "fish soup")])
		])
	])

--
-- Test functions
--

testP = parseRecipe fishsoup
testN = normalizeRecipe testP
testU = putStr (unparseRecipe testN)
testUnparse = putStr (unparseRecipe fishsoupRecipe)
testC = caloriesRecipe testN
testT = timeRecipe testN
testS = shoppinglist 10 testN
