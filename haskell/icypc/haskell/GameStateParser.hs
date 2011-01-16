module GameStateParser (
  module GameState
, module Text.ParserCombinators.Parsec

, parseGameState
)
where

import Text.ParserCombinators.Parsec
import GameState

parseGameState :: String -> GameState
parseGameState input = case result of
                         Left _      -> undefined
                         Right state -> state
    where result = parse gameState "(unknown)" input

gameState :: GenParser Char st GameState
gameState = do turnNumber <- turnNumberLine
               (scoreRed, scoreBlue) <- scoresLine
               fieldSpec <- fieldLines
               redChild1 <- childLine
               redChild2 <- childLine
               redChild3 <- childLine
               redChild4 <- childLine
               blueChild1 <- childLine
               blueChild2 <- childLine
               blueChild3 <- childLine
               blueChild4 <- childLine
               return (GameState turnNumber scoreRed scoreBlue (buildField fieldSpec) (buildTeam redChild1 redChild2 redChild3 redChild4) (buildTeam blueChild1 blueChild2 blueChild3 blueChild4))

turnNumberLine = do turnNumber <- number
                    newline
                    return turnNumber

scoresLine = do scoreRed <- number
                char ' '
                scoreBlue <- number
                newline
                return (scoreRed, scoreBlue)

fieldLines = count 31 fieldLine

fieldLine = do fields <- fieldContents `sepBy` (char ' ')
               newline
               return fields

fieldContents = fieldWithUnknownContents <|> fieldWithKnownContents

fieldWithUnknownContents = string "**" >> return Unknown

fieldWithKnownContents = do snowHeight <- digit
                            fieldContents <- oneOf ['a'..'j']
                            return (buildFieldSpace (read [snowHeight]) fieldContents)

childLine = do child <- childLineContents
               newline
               return child

childLineContents = unknownChildLine <|> knownChildLine

unknownChildLine = char '*' >> return InvisibleChild

knownChildLine = do posX <- number
                    char ' '
                    posY <- number
                    char ' '
                    standingOrCrouching <- oneOf "SC"
                    char ' '
                    holding <- oneOf ['a'..'i']
                    char ' '
                    dazedFor <- digit
                    return (buildChild posX posY standingOrCrouching holding (read [dazedFor]))

number = do s <- many1 digit
            return (read s)
