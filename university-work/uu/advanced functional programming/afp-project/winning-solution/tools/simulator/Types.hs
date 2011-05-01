
module Types (module Types, module Interface, module Data.Array,
              module Data.FiniteMap, module Language) where

import Data.Array
import Data.Array.Unboxed
import Data.FiniteMap
import Interface
import Language hiding (MarkID, CommandIndex)

newtype Map = Map (FiniteMap Pos Cell)

what_at :: FiniteMap Pos Cell -> Pos -> Cell
what_at a p = case lookupFM a p of
                  Nothing -> error "what_at: Can't happen"
                  Just x -> x

update_at :: FiniteMap Pos Cell -> Pos -> Cell -> FiniteMap Pos Cell
update_at fm x y = addToFM fm x y

newtype Ants = Ants (FiniteMap AntID Ant)
data Ant = Ant { a_pos :: !Pos,
                 a_col :: !Colour,
                 a_state :: !CommandIndex,
                 a_rest :: !Resting,
                 a_dir :: !Direction,
                 a_food :: !HasFood }

newtype Commands = Commands (Data.Array.Unboxed.Array CommandIndex Command)

type Direction = Int

data Type = Rocks | Anthill !Colour | Normal
    deriving (Show, Eq)

data Cell = Cell { c_type :: !Type,
                   c_ant :: !(Maybe AntID),
                   c_food :: !Amount,
                   c_redmarks :: !Marks,
                   c_blackmarks :: !Marks }
    deriving Show

data Marks = Marks { mark0 :: !Mark,
                     mark1 :: !Mark,
                     mark2 :: !Mark,
                     mark3 :: !Mark,
                     mark4 :: !Mark,
                     mark5 :: !Mark }
    deriving Show

type Mark = Bool

