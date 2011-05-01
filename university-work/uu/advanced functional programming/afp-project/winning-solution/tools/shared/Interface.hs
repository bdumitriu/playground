module Interface where

data VCommand = VStep    Int                                 -- timestep
              | VAnthill          Colour  Pos                -- anthill at
              | VRock                     Pos                -- rock at
              | VAnt      HasFood Colour  Pos  Dir     Resting AntID -- ant at
              | VNoAnt                    Pos                -- no ant at
              | VFood                     Pos  Amount        -- food at
              | VMark             Colour  Pos  MarkID        -- mark at
              | VUnMark           Colour  Pos  MarkID        -- unmark at
              | VScore    Score Score
              | VComment  AntID Pos String
	      | VProfile  { profileRedScore :: Score,
	                    profileBlackScore :: Score,
			    profileRedPickups :: Pickups,
			    profileBlackPickups :: Pickups,
			    profileRedDeaths :: Deaths,
			    profileBlackDeaths :: Deaths
	                  }
  deriving (Show,Read,Eq)

type Score = Int
type Pickups = Int  -- The total number of food items team has picked up thus far
type Deaths = Int

data Colour   = Red | Black
  deriving (Show,Read,Eq,Ord)

type Resting = Int

type HasFood = Bool

type X = Int
type Y = Int
-- type Pos      = (X, Y)
data Pos      = Pos !X !Y
  deriving (Show,Read,Eq)

instance Ord Pos where
    Pos x1 x2 <= Pos y1 y2 = (x2, x1) <= (y2, y1)

type Dir      = Int

type Amount   = Int

type MarkID   = Int

type AntID    = Int

type CommandIndex  = Int   -- 0-9999

