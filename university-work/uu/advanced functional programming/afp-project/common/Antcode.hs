-- | The "Antcode" module defines a few data types and a parser for 'Ant'
--   'Code'.
module Antcode (
  module Data.Array.IArray,
  Condition(..), Instruction(..), readAntFile, Code, Codes, _uselessAnts
  ) where

import Prelude hiding(Left, Right)
import Control.Arrow
import Data.Array.IArray
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

import Cell

-- | The kinds of 'Condition's for which an 'Ant' can check.
data Condition = Friend                 -- ^ 'Cell' contains an 'Ant' of the same 'Color'
               | Foe                    -- ^ 'Cell' contains an 'Ant' of the other 'Color'
               | FriendWithFood         -- ^ 'Cell' contains an 'Ant' of the same 'Color' carrying food
               | FoeWithFood            -- ^ 'Cell' contains an 'Ant' of the other 'Color' carrying food
               | Food                   -- ^ 'Cell' contains food (not being carried by an 'Ant')
               | Rock                   -- ^ 'Cell' is rocky
               | Marker Marker          -- ^ 'Cell' is marked with a 'Marker' of this 'Ant''s 'Color'
               | FoeMarker              -- ^ 'Cell' is marked with *some* marker of the other 'Color'
               | Home                   -- ^ 'Cell' belongs to this 'Ant''s anthill
               | FoeHome                -- ^ 'Cell' belongs to the other 'Color' of 'Ant' anthill
               deriving Eq

instance Show Condition where
  show Friend = "Friend"
  show Foe = "Foe"
  show FriendWithFood = "FriendWithFood"
  show FoeWithFood = "FoeWithFood"
  show Food = "Food"
  show Rock = "Rock"
  show (Marker x) = "Marker " ++ (show x) -- only important case
  show FoeMarker = "FoeMarker" 
  show Home = "Home"
  show FoeHome = "FoeHome"

-- | The types of 'Instruction's which can be in an 'Ant''s brain.
data Instruction = Sense SenseDir AntState AntState Condition   -- ^ sense for a 'Condition'
                 | Mark Marker AntState                         -- ^ mark current 'Cell' with a certain 'Marker'
                 | Unmark Marker AntState                       -- ^ unmark current 'Cell' of a certain 'Marker'
                 | PickUp AntState AntState                     -- ^ pick up food
                 | Drop AntState                                -- ^ drop food
                 | Turn LeftOrRight AntState                    -- ^ turn 'Left' or 'Right'
                 | Move AntState AntState                       -- ^ move according to current 'direction'
                 | Flip Int AntState AntState                   -- ^ flip a \"probabilistic coin\"
                 deriving (Eq, Show)

type Code = Array Int (Instruction, String)

-- | Default code to use if user doesn't supply code for the black 'Ant's.
--   According to this code, 'Ant's keep executing 'Drop' 'Instruction's in
--   order to simulate \"doing nothing\".
_uselessAnts :: Code
_uselessAnts = array (0, 0) [(0, (Drop 0, ""))]

-- | A pair of 'Code's: one for the first team, the other for the second team,
--   if it exists. The first is defined to be the 'Red' team.
type Codes = (Code, Maybe Code)

{- Functions for reading input -}

-- | Reads the contents of the file containing the states of an 'Ant''s brain.
readAntFile :: FilePath -> IO Code
readAntFile f = do r <- parseFromFile instructions f
                   ics <- (ioError . userError . show) ||| return $ r
                   return (listArray (0, (length ics) - 1) ics)

{- Parser for ant files -}

instructions :: Parser [(Instruction, String)]
instructions = do { ics <- instructionCom `sepEndBy` whiteSpace
                  ; eof
                  ; return ics
                  }

instructionCom :: Parser (Instruction, String)
instructionCom = do { i <- lexeme instruction
                    ; c <- option "" comment
                    ; return (i, c)
                    }

comment :: Parser String
comment = do { string ";"
             ; many (oneOf " \t")
             ; many (noneOf "\n\r")
             }

instruction :: Parser Instruction
instruction =     instructionSense
              <|> instructionMark
              <|> instructionUnmark
              <|> instructionPickUp
              <|> instructionDrop
              <|> instructionTurn
              <|> instructionMove
              <|> instructionFlip

instructionSense :: Parser Instruction
instructionSense = do { reserved "Sense"
                      ; sd <- sensedir
                      ; st1 <- st
                      ; st2 <- st
                      ; cd <- cond
                      ; return (Sense sd st1 st2 cd)
                      }

instructionMark :: Parser Instruction
instructionMark = do { reserved "Mark"
                     ; mk <- i
                     ; s <- st
                     ; return (Mark mk s)
                     }

instructionUnmark :: Parser Instruction
instructionUnmark = do { reserved "Unmark"
                       ; mk <- i
                       ; s <- st
                       ; return (Unmark mk s)
                       }

instructionPickUp :: Parser Instruction
instructionPickUp = do { reserved "PickUp"
                       ; st1 <- st
                       ; st2 <- st
                       ; return (PickUp st1 st2)
                       }

instructionDrop :: Parser Instruction
instructionDrop = do { reserved "Drop"
                     ; s <- st
                     ; return (Drop s)
                     }

instructionTurn :: Parser Instruction
instructionTurn = do { reserved "Turn"
                     ; dir <- lr
                     ; s <- st
                     ; return (Turn dir s)
                     }

instructionMove :: Parser Instruction
instructionMove = do { reserved "Move"
                     ; st1 <- st
                     ; st2 <- st
                     ; return (Move st1 st2)
                     }

instructionFlip :: Parser Instruction
instructionFlip = do { reserved "Flip"
                     ; num <- p
                     ; st1 <- st
                     ; st2 <- st
                     ; return (Flip num st1 st2)
                     }

sensedir :: Parser SenseDir
sensedir =     (reserved "Here" >> return Here)
           <|> (reserved "Ahead" >> return Ahead)
           <|> (reserved "LeftAhead" >> return LeftAhead)
           <|> (reserved "RightAhead" >> return RightAhead)

cond :: Parser Condition
cond =     (reserved "Friend" >> return Friend)
       <|> (reserved "Foe" >> return Foe)
       <|> (reserved "FriendWithFood" >> return FriendWithFood)
       <|> (reserved "FoeWithFood" >> return FoeWithFood)
       <|> (reserved "Food" >> return Food)
       <|> (reserved "Rock" >> return Rock)
       <|> (reserved "Marker" >> i >>= return . Marker)
       <|> (reserved "FoeMarker" >> return FoeMarker)
       <|> (reserved "Home" >> return Home)
       <|> (reserved "FoeHome" >> return FoeHome)

lr :: Parser LeftOrRight
lr =     (reserved "Left" >> return Left)
     <|> (reserved "Right" >> return Right)

st :: Parser AntState
st = lexeme decimal >>= return . fromInteger

i :: Parser Marker
i = lexeme (oneOf "012345") >>= return . read . (:[])

p :: Parser Int
p = lexeme decimal >>= return . fromInteger

{- Lexer for ant files -}

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
          emptyDef { --P.commentLine   = ";",
                     P.reservedNames = ["Sense", "Mark", "Unmark", "PickUp", "Drop", "Turn", "Move", "Flip",
                                        "Here", "Ahead", "LeftAhead", "RightAhead",
                                        "Friend", "Foe", "FriendWithFood", "FoeWithFood", "Food", "Rock",
                                        "Marker", "FoeMarker", "Home", "FoeHome", "Left", "Right" ]
                   }

reserved :: String -> CharParser () ()
reserved = P.reserved lexer

decimal :: CharParser () Integer
decimal = P.decimal lexer

whiteSpace :: CharParser () ()
whiteSpace = P.whiteSpace lexer

lexeme :: CharParser () a -> CharParser () a
lexeme = P.lexeme lexer

semi :: CharParser () String
semi = P.semi lexer

charLiteral :: CharParser () Char
charLiteral = P.charLiteral lexer