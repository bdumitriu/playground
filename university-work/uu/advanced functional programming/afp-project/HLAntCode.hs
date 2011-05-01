-- | Defines the datatypes for high
--   level ant code.
module HLAntCode where

import Antcode
import Ant
import Cell
import Prelude hiding(Left, Right, drop, break, flip)

-- | The possible High Level Instructions.
data HLI = CDrop                            -- ^ 1-to-1 match of the low level Drop instruction
    | CTurn LeftOrRight                     -- ^ 1-to-1 match of the low level Turn instruction
    | CMark Marker                          -- ^ 1-to-1 match of the low level Mark instruction
    | CUnmark Marker                        -- ^ 1-to-1 match of the low level Unmark instruction
    | CNop                                  -- ^ do nothing (usu. useful in branches of an if-then-else)
    | CBreak                                -- ^ break after the innermost do-until
    | CSeq HLI HLI                          -- ^ run first HLI, then second HLI
    | CIfThenElse Cond HLI HLI              -- ^ if Cond is successful, execute first HLI, else second HLI
    | CDoUntil Cond HLI                     -- ^ repeat HLI until Cond is successful
    | CWhile Cond HLI                       -- ^ repeat HLI when Cond is successful, less efficient than CDoUntil
    | Label String                          -- ^ label the current step
    | Goto String                           -- ^ jump to the given step
    deriving (Eq, Show)

-- | Possible conditions for an 'CIfThenElse' or for a 'CDoUntil'.
data Cond = CSense SenseDir Condition      -- ^ 1-to-1 match of the low level Sense instruction
     | CMove                               -- ^ 1-to-1 match of the low level Move instruction
     | CPickUp                             -- ^ 1-to-1 match of the low level PickUp instruction
     | CFlip Int                           -- ^ 1-to-1 match of the low level Flip instruction
     | CNot Cond                           -- ^ Not boolean logic.
     | CAnd Cond Cond                      -- ^ And
     | COr Cond Cond                       -- ^ Or
     | CTrue                               -- ^ True
     | CFalse                              -- ^ False
     deriving (Eq, Show)