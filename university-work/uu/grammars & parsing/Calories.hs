module Calories where

type KCal = Float

-- per 100 g or per dl.
-- Add as many additional products as you like yourself, and
-- try to find realistic calorie values.

kcalTable :: [(String, KCal)]
kcalTable =  [("bread", 153)		-- slice
             ,("fish bouillon", 800)	-- block
             ,("codfish", 200)
             ,("prawns", 310)
             ,("salt", 0)
             ,("water", 0)
	     ,("flour", 200)
	     ,("butter", 800)
	     ,("egg", 300)		-- one egg
	     ,("eggs", 300)		-- one egg
	     ,("whipped cream", 300)
	     ,("cocoa", 20)		-- spoonful
	     ,("powdered sugar", 400)
	     ,("fresh pasta", 400)
	     ,("tomato blocks", 300)	-- tin
	     ,("parmesan cheese", 400)	-- bag
	     ,("onion", 50)		-- one onion
	     ,("onions", 50)		-- one onion
             ]
