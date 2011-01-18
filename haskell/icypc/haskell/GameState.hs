module GameState (
  module Data.Array

, Score

, FieldContents(..)
, buildFieldContents

, FieldSpace(..)
, buildFieldSpace
, buildKnownFieldSpace
, buildUnknownFieldSpace
, updateInfoAgeInFieldSpace
, incInfoAgeInFieldSpace
, updateChildInFieldSpace
, snowHeightAt

, Field
, buildField

, ChildPayload(..)
, buildChildPayload

, Child(..)
, buildChild

, Team
, buildTeam
, getChild1
, getChild2
, getChild3
, getChild4
, getChildren
, getChildrenAsList
, getUndazedChildren
, getDazedChildren

, GameState(..)
, updateFieldInGameState
) where

import Data.Array

type Score = Int

data FieldContents = Unknown
                   | Empty Int
                   | Tree Int
                   | S_SnowBall Int
                   | M_SnowBall Int
                   | MS_SnowBall Int
                   | L_SnowBall Int
                   | LM_SnowBall Int
                   | LS_SnowBall Int
                   | RedSnowman Int
                   | BlueSnowman Int
                     deriving (Eq, Show)

data FieldSpace = FieldSpace {
      infoAge  :: Int
    , childIn  :: Maybe Child
    , contents :: FieldContents
} deriving (Eq, Show)

type Field = Array (Int, Int) FieldSpace

data ChildPayload = NotMuch
                  | Snow Int
                  | SSnowBall Int
                  | MSnowBall
                  | LSnowBall
                    deriving (Eq, Show)

data Child = Child {
      position     :: (Int, Int)
    , isStanding   :: Bool
    , payload      :: ChildPayload
    , dazedAnother :: Int
} deriving (Eq, Show)

type Team = [Maybe Child]

data GameState = GameState {
      turnNumber   :: Int
    , scoreRed     :: Score
    , scoreBlue    :: Score
    , field        :: Field
    , redChildren  :: Team
    , blueChildren :: Team
} deriving (Eq, Show)

buildFieldContents :: Int -> Char -> FieldContents
buildFieldContents snowHeight 'a' = Empty snowHeight
buildFieldContents snowHeight 'b' = Tree snowHeight
buildFieldContents snowHeight 'c' = S_SnowBall snowHeight
buildFieldContents snowHeight 'd' = M_SnowBall snowHeight
buildFieldContents snowHeight 'e' = MS_SnowBall snowHeight
buildFieldContents snowHeight 'f' = L_SnowBall snowHeight
buildFieldContents snowHeight 'g' = LM_SnowBall snowHeight
buildFieldContents snowHeight 'h' = LS_SnowBall snowHeight
buildFieldContents snowHeight 'i' = RedSnowman snowHeight
buildFieldContents snowHeight 'j' = BlueSnowman snowHeight

buildFieldSpace :: FieldContents -> FieldSpace
buildFieldSpace = FieldSpace 0 Nothing

buildKnownFieldSpace :: Int -> Char -> FieldSpace
buildKnownFieldSpace snowHeight = buildFieldSpace . buildFieldContents snowHeight

buildUnknownFieldSpace :: FieldSpace
buildUnknownFieldSpace = buildFieldSpace Unknown

updateInfoAgeInFieldSpace :: FieldSpace -> Int -> FieldSpace
updateInfoAgeInFieldSpace (FieldSpace _ c cts) newInfoAge = FieldSpace newInfoAge c cts

incInfoAgeInFieldSpace :: FieldSpace -> FieldSpace
incInfoAgeInFieldSpace (FieldSpace a c cts) = FieldSpace (a + 1) c cts

updateChildInFieldSpace :: FieldSpace -> Maybe Child -> FieldSpace
updateChildInFieldSpace (FieldSpace a _ c) newChild = FieldSpace a newChild c

snowHeightAtFC :: FieldContents -> Maybe Int
snowHeightAtFC Unknown         = Nothing
snowHeightAtFC (Empty h)       = Just h
snowHeightAtFC (Tree h)        = Just h
snowHeightAtFC (S_SnowBall h)  = Just h
snowHeightAtFC (M_SnowBall h)  = Just h
snowHeightAtFC (MS_SnowBall h) = Just h
snowHeightAtFC (L_SnowBall h)  = Just h
snowHeightAtFC (LM_SnowBall h) = Just h
snowHeightAtFC (LS_SnowBall h) = Just h
snowHeightAtFC (RedSnowman h)  = Just h
snowHeightAtFC (BlueSnowman h) = Just h

snowHeightAt :: FieldSpace -> Maybe Int
snowHeightAt = snowHeightAtFC . contents

buildField :: [[FieldSpace]] -> Field
buildField fieldSpaces = array ((0, 0), (30, 30)) (zip ranges (concat fieldSpaces))
    where ranges = range ((0, 0), (30, 30))

buildChildPayload :: Char -> ChildPayload
buildChildPayload 'a' = NotMuch
buildChildPayload 'b' = Snow 1
buildChildPayload 'c' = Snow 2
buildChildPayload 'd' = Snow 3
buildChildPayload 'e' = SSnowBall 1
buildChildPayload 'f' = SSnowBall 2
buildChildPayload 'g' = SSnowBall 3
buildChildPayload 'h' = MSnowBall
buildChildPayload 'i' = LSnowBall

childIsStanding :: Char -> Bool
childIsStanding 'S' = True
childIsStanding 'C' = False

buildChild :: Int -> Int -> Char -> Char -> Int -> Maybe Child
buildChild posX posY standingOrCrouching payload dazedAnother = Just (Child (posX, posY) (childIsStanding standingOrCrouching) (buildChildPayload payload) dazedAnother)

buildTeam :: Maybe Child -> Maybe Child -> Maybe Child -> Maybe Child -> Team
buildTeam c1 c2 c3 c4 = [c1, c2, c3, c4]

getChild1 :: Team -> Maybe Child
getChild1 (c:_) = c

getChild2 :: Team -> Maybe Child
getChild2 (_:c:_) = c

getChild3 :: Team -> Maybe Child
getChild3 (_:_:c:_) = c

getChild4 :: Team -> Maybe Child
getChild4 (_:_:_:c:[]) = c

getChildren :: Team -> (Maybe Child, Maybe Child, Maybe Child, Maybe Child)
getChildren (c1:c2:c3:c4:[]) = (c1, c2, c3, c4)

getChildrenAsList :: Team -> [Maybe Child]
getChildrenAsList t = t

getUndazedChildren :: Team -> [Maybe Child]
getUndazedChildren = filter predicateUndazed
    where predicateUndazed :: Maybe Child -> Bool
          predicateUndazed (Just c) = dazedAnother c == 0
          predicateUndazed Nothing  = False

getDazedChildren :: Team -> [Maybe Child]
getDazedChildren = filter predicateDazed
    where predicateDazed :: Maybe Child -> Bool
          predicateDazed (Just c) = dazedAnother c > 0
          predicateDazed Nothing  = False

updateFieldInGameState :: GameState -> Field -> GameState
updateFieldInGameState (GameState t sr sb _ rc bc) newField = GameState t sr sb newField rc bc
