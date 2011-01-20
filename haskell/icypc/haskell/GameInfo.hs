module GameInfo (
  module GameState

, GameInfo(..)
, emptyGameInfo
, registerNewState
, currentState
, currentBareState
) where

import GameState

data GameInfo = GameInfo {
      stateHistory     :: [GameState]
    , bareStateHistory :: [GameState]
} deriving (Eq, Show)

emptyGameInfo :: GameInfo
emptyGameInfo = GameInfo [] []

currentState :: GameInfo -> GameState
currentState = head . stateHistory

currentBareState :: GameInfo -> GameState
currentBareState = head . bareStateHistory

registerNewState :: GameInfo -> GameState -> GameInfo
registerNewState gameInfo newGameState
 | history == [] = GameInfo [addChildrenToField newGameState] [newGameState]
 | otherwise     = GameInfo ((augmentGameState (currentState gameInfo) newGameState) : history) (newGameState : bareHistory)
    where history     = stateHistory gameInfo
          bareHistory = bareStateHistory gameInfo

augmentGameState :: GameState -> GameState -> GameState
augmentGameState oldState newState = updateFieldInGameState stateWithChildren (augmentField (field oldState) (field stateWithChildren))
    where stateWithChildren = addChildrenToField newState

addChildrenToField :: GameState -> GameState
addChildrenToField gameState = foldr addTeamToField gameState [redChildren gameState, blueChildren gameState]

addTeamToField :: Team -> GameState -> GameState
addTeamToField team gameState = foldr addChildToField gameState team

addChildToField :: Maybe Child -> GameState -> GameState
addChildToField (Just child) gameState = addVisibleChildToField child gameState
addChildToField Nothing gameState      = gameState

addVisibleChildToField :: Child -> GameState -> GameState
addVisibleChildToField child gameState = updateFieldInGameState gameState newField
    where oldField = field gameState
          childPosition = position child
          oldFieldSpace = oldField!childPosition
          newField = oldField // [(childPosition, (updateChildInFieldSpace oldFieldSpace (Just child)))]

augmentField :: Field -> Field -> Field
augmentField oldField newField = newField // changes
    where changes = foldr fieldChange []  (zip3 (indices oldField) (elems oldField) (elems newField))
          fieldChange :: ((Int, Int), FieldSpace, FieldSpace) -> [((Int, Int), FieldSpace)] -> [((Int, Int), FieldSpace)]
          fieldChange (pos, oldFieldSpace, (FieldSpace _ _ Unknown)) cs = (pos, incInfoAgeInFieldSpace oldFieldSpace) : cs
          fieldChange _ cs                                            = cs
