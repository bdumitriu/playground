module GameState (
  module Data.Array

, Score

, FieldSpace(..)
, buildFieldSpace
, snowHeightAt

, Field
, buildField

, ChildPayload(..)
, buildChildPayload

, ChildInfo(..)
, childIsStanding

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
) where

import Data.Array

type Score = Int

data FieldSpace = Unknown
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

type Field = Array (Int, Int) FieldSpace

data ChildPayload = NotMuch
                  | Snow Int
                  | SSnowBall Int
                  | MSnowBall
                  | LSnowBall
                    deriving (Eq, Show)

data ChildInfo = ChildInfo {
      position     :: (Int, Int)
    , isStanding   :: Bool
    , holds        :: ChildPayload
    , dazedAnother :: Int
} deriving (Eq, Show)

data Child = InvisibleChild
           | VisibleChild ChildInfo
             deriving (Eq, Show)

type Team = [Child]

data GameState = GameState {
      turnNumber   :: Int
    , scoreRed     :: Score
    , scoreBlue    :: Score
    , field        :: Field
    , redChildren  :: Team
    , blueChildren :: Team
} deriving (Eq, Show)

buildFieldSpace :: Int -> Char -> FieldSpace
buildFieldSpace snowHeight 'a' = Empty snowHeight
buildFieldSpace snowHeight 'b' = Tree snowHeight
buildFieldSpace snowHeight 'c' = S_SnowBall snowHeight
buildFieldSpace snowHeight 'd' = M_SnowBall snowHeight
buildFieldSpace snowHeight 'e' = MS_SnowBall snowHeight
buildFieldSpace snowHeight 'f' = L_SnowBall snowHeight
buildFieldSpace snowHeight 'g' = LM_SnowBall snowHeight
buildFieldSpace snowHeight 'h' = LS_SnowBall snowHeight
buildFieldSpace snowHeight 'i' = RedSnowman snowHeight
buildFieldSpace snowHeight 'j' = BlueSnowman snowHeight

snowHeightAt :: FieldSpace -> Maybe Int
snowHeightAt Unknown         = Nothing
snowHeightAt (Empty h)       = Just h
snowHeightAt (Tree h)        = Just h
snowHeightAt (S_SnowBall h)  = Just h
snowHeightAt (M_SnowBall h)  = Just h
snowHeightAt (MS_SnowBall h) = Just h
snowHeightAt (L_SnowBall h)  = Just h
snowHeightAt (LM_SnowBall h) = Just h
snowHeightAt (LS_SnowBall h)  = Just h
snowHeightAt (RedSnowman h)  = Just h
snowHeightAt (BlueSnowman h) = Just h

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

buildChild :: Int -> Int -> Char -> Char -> Int -> Child
buildChild posX posY standingOrCrouching payload dazedFor = VisibleChild (ChildInfo (posX, posY) (childIsStanding standingOrCrouching) (buildChildPayload payload) dazedFor)

buildTeam :: Child -> Child -> Child -> Child -> Team
buildTeam c1 c2 c3 c4 = [c1, c2, c3, c4]

getChild1 :: Team -> Child
getChild1 (c:_) = c

getChild2 :: Team -> Child
getChild2 (_:c:_) = c

getChild3 :: Team -> Child
getChild3 (_:_:c:_) = c

getChild4 :: Team -> Child
getChild4 (_:_:_:c:[]) = c

getChildren :: Team -> (Child, Child, Child, Child)
getChildren (c1:c2:c3:c4:[]) = (c1, c2, c3, c4)

getChildrenAsList :: Team -> [Child]
getChildrenAsList t = t

getUndazedChildren :: Team -> [Child]
getUndazedChildren = filter predicateUndazed
    where predicateUndazed :: Child -> Bool
          predicateUndazed (VisibleChild c) = dazedAnother c == 0
          predicateUndazed InvisibleChild = False

getDazedChildren :: Team -> [Child]
getDazedChildren = filter predicateDazed
    where predicateDazed :: Child -> Bool
          predicateDazed (VisibleChild c) = dazedAnother c > 0
          predicateDazed InvisibleChild = False
