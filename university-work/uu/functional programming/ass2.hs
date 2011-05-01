{-
	Assignment 2
	Bogdan Dumitriu
	26 September 2004
-}

{-
	Provided definition and tables.
-}

data Table = Table
	String		-- table name
	[String]	-- field names
	[[String]]	-- records

top10 = Table
	"Top10"
	["Nr", "Artist", "Clip"]
	[
		["1", "Shakira", "Whenever Wherever"],
		["2", "Marco en Sita", "Lopen op het water"],
		["3", "Pink", "Get the Party Started"],
		["4", "Within Temptation", "Ice Queen"],
		["5", "Linkin Park", "In the End"],
		["6", "Gigi d'Agostino", "L'Amour Toujours"],
		["7", "Nelly Furtado", "Shit on the Radio"],
		["8", "Nickelback", "How You Remind Me"],
		["9", "Alanis Morissette", "Hands Clean"],
		["10", "Kane", "Let It Be"]
	]

origin = Table
	"Origin"
	["Artist", "Land"]
	[
		["Marco en Sita", "Netherland"],
		["Marco en Sita", "Italy"],
		["Alanis Morissette", "Canada"],
		["Kane", "Netherland"],
		["Within Temptation", "Netherland"],
		["Shakira", "Colombia"],
		["Pink", "United States"]
	]

lands = Table
	"Lands"
	["Land", "Population"]
	[
		["Netherland", "16,000,000"],
		["Romania", "22,000,000"],
		["United States", "230,000,000"],
		["China", "1,300,000,000"],
		["Canada", "32,000,000"],
		["India", "1,000,000,000"],
		["Russia", "145,000,000"],
		["Colombia", "44,500,000"],
		["Germany", "82,000,000"],
		["France", "62,300,000"],
		["Spain", "42,700,000"],
		["Italy", "58,000,000"],
		["Portugal", "10,500,000"]
	]

{-
	Functions to return each of the three components of a Table.
-}

tName :: Table -> String
tName (Table tn _ _) = tn

fNames :: Table -> [String]
fNames (Table _ fn _) = fn

recs :: Table -> [[String]]
recs (Table _ _ rec) = rec

{-
	Functionality 1: Showing a Table.
-}

printTable :: Table -> IO()
printTable = putStr . writeTable

writeTable :: Table -> String
writeTable table =
	tn ++ ":\n" ++								-- print table name
	(if lfn == 0 then "||\n" else fNames) ++				-- print field names
	(if lfn == 0 then "--" else replicate (length fNames-1) '-') ++ "\n" ++	-- print the separating line(s)
	concat (map as rec)							-- print the records
  where
  (Table tn fn rec)	= padWithSpaces table (maxFieldLength table)
  lfn			= length fn
  fNames		= as fn
  as x			= (concat . addSeparators "|") x ++ "\n"

-- given a table and a list of maximum field sizes (see description of
-- function maxFieldLenght for this) it will pad both the filed names
-- and the String's from the records with the right amount of spaces
-- so that every field name and every String from every record will
-- have the length equal to the maximum size for the field
padWithSpaces :: Table -> [Int] -> Table
padWithSpaces (Table tn fn rec) fsize = (Table tn (zipWith pad fsize fn) (map (zipWith pad fsize) rec))

-- given an Int and a String, returns the same String padded with spaces to
-- its right so that its resulting length (after the padding operation) is
-- equal to the Int supplied as parameter. If the String's length is already
-- equal to (or exceeds) the supplied one, the String will be returned unchanged
pad :: Int -> String -> String
pad n st
  | len < n	= st ++ (replicate (n-len) ' ')
  | len >= n	= st
  where
  len = length st

-- returns the maximum length of the String's (taken over all records in the
-- table) of each field. The field names themselves are also taken into account
-- in computing the maximum value
maxFieldLength :: Table -> [Int]
maxFieldLength (Table _ fn rec) = foldr posmax (replicate (length fn) 0) (map lengths (fn:rec))

-- given a list of lists, returns a list which contains the length of each list
lengths :: [[a]] -> [Int]
lengths = map length

-- given two lists of Int's, returns the list with "positional maximums", i.e.
-- list containing the max of each pair of type (x, y), with x being the i-th
-- element of the first list and y being the i-th element of the second list.
-- If the two list have different lengths, the resulting list will have the
-- length of the shorter one
posmax :: [Int] -> [Int] -> [Int]
posmax = zipWith max

-- adds a separator (the first argument) between every elements in the list
-- (the second argument), including one separator before the first element
-- and one after the last element and returns the new list
addSeparators :: a -> [a] -> [a]
addSeparators _ []	= []
addSeparators sep list	= sep : (concat (map (\x -> [x, sep]) list))

{-
	Functionality 2: Selecting Columns.
-}

transpose :: [[a]] -> [[a]]
transpose []= []
transpose [x]= map (\x -> [x]) x
transpose (x:s)= zipWith (:) x (transpose s)

project :: [String] -> Table -> Table
--project sfn (Table tn fn rec)
--  | selIndices == []	= Table tn [] []
--  | otherwise		= Table tn (filterIndices selIndices fn) (map (filterIndices selIndices) rec)
--  where
--  selIndices = getFieldIndices sfn (Table tn fn rec)



project sfn (Table tn fn rec) = Table tn fn' rec'
   where
   (fn':rec')  =  transpose . filter (elem sfn . head) . transpose $ (fn : rec)

-- given a list of indices (0-based) and another list, returns those elements of
-- the second list which are on the positions indicated by the Int's in the first
-- list (e.g. filterIndices [3, 5, 6] ['a', 'b', 'c', 'd', 'e', 'f', 'g'] will
-- return ['d', 'f', 'g'])
filterIndices :: [Int] -> [a] -> [a]
filterIndices indices list = snd (unzip (filter (\(idx, _) -> elem idx indices) (zip [0 .. ] list)))

-- given a list of field names and a table, returns a list of the indices
-- of the fields (0-based) which are in the list of field names
-- (e.g. getFieldIndices ["artist", "song"] (Table _ ["nr", "artist", "song"] _)
-- will return [1, 2])
getFieldIndices :: [String] -> Table -> [Int]
getFieldIndices sfn (Table _ fn _) = fst (unzip (filter (\(_, fName) -> elem fName sfn) (zip [0 .. ] fn)))

{-
	Functionality 3: Selecting Records.
-}

select :: String -> (String -> Bool) -> Table -> Table
select fName p (Table tn fn rec)
  | idx == -1	= Table tn fn []
  | otherwise	= Table tn fn (filter (\x -> p (x!!idx)) rec)
  where
  idx = getFieldIndex fName (Table tn fn rec)

-- given a field name and a table, returns the index of the field (0-based)
-- which corresponds to the field name. The function returns -1 if no field
-- with the supplied name exists. As an example,
-- getFieldIndex "artist" (Table _ ["nr", "artist", "song"] _) will return 1.
getFieldIndex :: String -> Table -> Int
getFieldIndex sfn table
  | indices == []	= -1
  | otherwise		= head indices
  where
  indices = getFieldIndices [sfn] table

{-
	Functionality 4: Combining Tables.
-}

join :: Table -> Table -> Table
join (Table fTn fFn fRec) (Table sTn sFn sRec)
  | comField == ""	= (Table (fTn ++ " " ++ sTn) (fFn ++ sFn) [])
  | otherwise		= (Table (fTn ++ " " ++ sTn) (putTogether fFn sFn sIdx) [putTogether fR sR sIdx | fR <- fRec, sR <- sRec, fR!!fIdx == sR!!sIdx])
  where
  comField	= commonField (Table fTn fFn fRec) (Table sTn sFn sRec)
  fIdx		= getFieldIndex comField (Table fTn fFn fRec)
  sIdx		= getFieldIndex comField (Table sTn sFn sRec)

-- given two lists and an index (0-based) in the second list, returns the list
-- obtained by concatenating the first two lists after dropping the element on the
-- position indicated by the index from the second list
putTogether :: [a] -> [a] -> Int -> [a]
putTogether f s sIdx = f ++ (take (sIdx-1) s) ++ (drop (sIdx+1) s)

-- finds the common field of the two tables and returns it. If more than one
-- common field exist, only the first one (in the order in which they appear in
-- the second table) is returned. If no common fields exist, the empty string
-- is returned
commonField :: Table -> Table -> String
commonField (Table _ fFn _) (Table _ sFn _)
  | commonFields == []	= ""
  | otherwise		= head commonFields
  where
  commonFields = (filter (\x -> elem x fFn) sFn)

query = project ["Land"] . select "Nr" isTop3 $ join top10 origin
  where
  isTop3 ranking = read ranking <= (3::Int)
