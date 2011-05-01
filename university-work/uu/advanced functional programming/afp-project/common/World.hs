-- | The "World" module defines most of the functions that form the simulator.
--   It also defines the top level data structures that are used to simulate
--   the 'World'.
--
--   For detailed comments about most of the functions in this module, you are
--   referred to this document: <http://www.cis.upenn.edu/proj/plclub/contest/ants.html>.
module World (
  module World, module Cell, module Antcode,
  getAssocs
  ) where

import Cell
import Antcode

import Prelude hiding (Left, Right)
import Data.Bits
import Data.Array.IO
import Data.Ix
import Control.Monad.State hiding (State)
import Graphics.UI.WX.Types hiding (Color)

-- | A position on the hexagonal board. The first 'Int' encodes the column,
--   the second one encodes the line.
data Pos = Pos Int Int
         deriving Eq

instance Show Pos where
  show (Pos x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Ord Pos where
  (<=) (Pos a b) (Pos c d) = (b, a) <= (d, c)

instance Ix Pos where
  range (Pos a b, Pos c d) = map (\(x, y) -> Pos y x) $ range ((b, a), (d, c))
  index (Pos a b, Pos c d) (Pos e f) = index ((b, a), (d, c)) (f, e)
  inRange (Pos a b, Pos c d) (Pos e f) = inRange ((b, a), (d, c)) (f, e)

-- | The board is represented as a mutable array of 'Cell's, indexed by
--   'Pos'ition.
type Board = IOArray Pos Cell

-- | Some data about each 'Cell' which is cached so that the 'Visualizer'
--   doesn't have to recompute it every time it repains the 'board'. This
--   should lead to some speedup in GUI response.
data CacheData = CD { center_x   :: Float
                    , center_y   :: Float
                    , hexPoints  :: [Point]
                    , dir0Points :: [Point]
                    , dir1Points :: [Point]
                    , dir2Points :: [Point]
                    , dir3Points :: [Point]
                    , dir4Points :: [Point]
                    , dir5Points :: [Point]
                    }

-- | The 'Cache' associates a 'CacheData' record with each 'Pos'ition.
type Cache = Array Pos CacheData

-- | Each 'AntId' is associated with the 'Pos'ition of that ant on the board.
--   If an 'Ant' ceases its existence, its position will be represented by the
--   'Nothing' value.
type Ants = IOArray AntId (Maybe Pos)

-- | A collection of structures that form the state of the world.
data WorldState = WorldState { board :: Board   -- ^ the hexagonal 'Board'
                             , cache :: Cache   -- ^ the cache for the visualizer
                             , ants  :: Ants    -- ^ the array of 'Ant' 'Pos'itions on the board
                             , codes :: Codes   -- ^ the 'Codes' of the red and the black 'Ant's
                             , rng   :: [Int]   -- ^ the list of random numbers
                             , round :: Int     -- ^ the number of the current round
                             , o     :: Bool    -- ^ trigger to output or not
                             , rds   :: Int     -- ^ total number of rounds to run
                             }

-- | The world is represented as a 'StateT' monad which uses 'WorldState' as
--   its state and which contains an IO monad inside, since we make (heavy) use
--   of 'IOArray's in our representation.
type World a = StateT WorldState IO a

-- | Creates a new 'WorldState' instance.
newWorldState :: Board  -- ^ the hexagonal 'Board'
        -> Ants         -- ^ the array of 'Ant' 'Pos'itions on the board
        -> Code         -- ^ the 'Code' for the red 'Ant's
        -> Code         -- ^ the 'Code' for the black 'Ant's
        -> Int          -- ^ the seed for the random number generator
        -> Bool         -- ^ trigger to output or not
        -> Int          -- ^ total number of rounds to run
        -> Maybe Int    -- ^ the size of the hexagon representing a 'Cell'
                        --   if a visualizer is used. If the value is 'Nothing',
                        --   then the 'cache' will be initialized to an
                        --   empty 'IOArray'
        -> WorldState   -- ^ the generated 'WorldState'
newWorldState b a c1 c2 s o' rds' l = WorldState { board = b
                                                 , cache = mkCache l (bounds b)
                                                 , ants  = a
                                                 , codes = (c1, Just c2)
                                                 , rng   = mkRng s
                                                 , World.round = 0
                                                 , o = o'
                                                 , rds = rds'
                                                 }

mkCache :: Maybe Int -> (Pos, Pos) -> Cache
mkCache Nothing  _  = listArray (Pos 1 1, Pos 0 0) []
mkCache (Just l) ps = listArray ps (map (mkCacheData l) (range ps))

mkCacheData :: Int -> Pos -> CacheData
mkCacheData l (Pos x y) = CD { center_x   = cx
                             , center_y   = cy
                             , hexPoints  = hPoints l cx cy x_delta y_delta
                             , dir0Points = dPoints 0 l cx cy
                             , dir1Points = dPoints 1 l cx cy
                             , dir2Points = dPoints 2 l cx cy
                             , dir3Points = dPoints 3 l cx cy
                             , dir4Points = dPoints 4 l cx cy
                             , dir5Points = dPoints 5 l cx cy
                             }
  where x_delta = sin (pi / 3.0) * fromIntegral l
        y_delta = fromIntegral l / 2
        cx      = fromIntegral (2 * fromIntegral x + 1 + (y `mod` 2)) * x_delta
        cy      = fromIntegral y * (fromIntegral l + y_delta) + fromIntegral l

hPoints :: Int -> Float -> Float -> Float -> Float
        -> [Point]      -- ^ the list of 'Point's denoting the hexagon's corners
hPoints l center_x center_y x_delta y_delta =
  [ Point (Prelude.round (center_x          )) (Prelude.round (center_y - fromIntegral l))      -- 12 o'clock
  , Point (Prelude.round (center_x + x_delta)) (Prelude.round (center_y - y_delta))             -- 2  o'clock
  , Point (Prelude.round (center_x + x_delta)) (Prelude.round (center_y + y_delta))             -- 4  o'clock
  , Point (Prelude.round (center_x          )) (Prelude.round (center_y + fromIntegral l))      -- 6  o'clock
  , Point (Prelude.round (center_x - x_delta)) (Prelude.round (center_y + y_delta))             -- 8  o'clock
  , Point (Prelude.round (center_x - x_delta)) (Prelude.round (center_y - y_delta))             -- 10  o'clock
  ]

dPoints :: Dir -> Int -> Float -> Float -> [Point]
dPoints dir l center_x center_y =
  map mkPoint [(x1, y1), (x2, y2), (x3, y3)]
  where x1 cx l = map (cx +) [l2, y_delta, -y_delta, -l2, -y_delta, y_delta]
        y1 cy l = map (cy +) [0, x_delta, x_delta, 0, -x_delta, -x_delta]
        x2 cx l = map (cx +) [-x_delta, -x_delta, 0, x_delta, x_delta, 0]
        y2 cy l = map (cy +) [y_delta, -y_delta, -l2, -y_delta, y_delta, l2]
        x3 cx l = map (cx +) [-x_delta, 0, x_delta, x_delta, 0, -x_delta]
        y3 cy l = map (cy +) [-y_delta, -l2, -y_delta, y_delta, l2, y_delta]

        l2      = fromIntegral l * (2 / 3)
        x_delta = cos (pi / 6.0) * l2
        y_delta = sin (pi / 6.0) * l2

        mkPoint (x, y) = Point (Prelude.round (x center_x l !! dir)) (Prelude.round (y center_y l !! dir))

{- Functions for reading input -}

-- | Reads the contents of the file containing the board and packs it in a
--   'Board' object, which it returns. Additionally, it also creates an 'Ants'
--   'IOArray' based on the 'Ant's on the board, which it also returns.
readBoardFile :: FilePath -> IO (Board, Ants)
readBoardFile f = do c <- readFile f
                     (xx:yy:cc) <- return (words c)
                     x <- readIO xx
                     y <- readIO yy
                     (cs, i) <- return (runState (newCells (map head cc)) 0)
                     bd <- newListArray (Pos 0 0, Pos (x-1) (y-1)) cs
                     antlist <- mkListAntFromBoard bd
                     as <- newListArray (0, i-1) antlist
                     return (bd, as)

-- | Traverses the 'Board' and returs a list with the 'Pos'itions on the
--   'Board' occupied by 'Ant's.
mkListAntFromBoard :: Board -> IO [Maybe Pos]
mkListAntFromBoard b = do ass <- getAssocs b
                          return [Just pos | (pos, cell) <- ass, hasAnt cell]

getBoard :: World Board
getBoard = get >>= return . board

getAnts :: World Ants
getAnts = get >>= return . ants

getCodes :: World Codes
getCodes = get >>= return . codes

getRng :: World [Int]
getRng = get >>= return . rng

getRound :: World Int
getRound = get >>= return . World.round

getO :: World Bool
getO = get >>= return . o

getRds :: World Int
getRds = get >>= return . rds

updateRound :: World ()
updateRound = modify (\x -> x { World.round = succ (World.round x) })

putBoard :: Board -> World ()
putBoard b = do x <- get
                put (x { board = b } )

putAnts :: Ants -> World ()
putAnts a = do x <- get
               put (x { ants = a } )

putCodes :: Codes -> World ()
putCodes c = do x <- get
                put (x { codes = c } )

putRng :: [Int] -> World ()
putRng i = do x <- get
              put (x { rng = i } )

cellAt :: Pos -> World Cell
cellAt p = do w <- getBoard
              liftIO $ readArray w p

{- Geometry functions -}

adjacentCell :: Pos -> Dir -> Pos
adjacentCell (Pos x y) d =
  case d of
    0 -> Pos (x+1) y
    1 -> if (even y) then Pos x (y+1) else Pos (x+1) (y+1)
    2 -> if (even y) then Pos (x-1) (y+1) else Pos x (y+1)
    3 -> Pos (x-1) y
    4 -> if (even y) then Pos (x-1) (y-1) else Pos x (y-1)
    5 -> if (even y) then Pos x (y-1) else Pos (x+1) (y-1)
    _ -> error "inexistent direction"

turn :: LeftOrRight -> Dir -> Dir
turn Left d  = (d+5) `mod` 6
turn Right d = (d+1) `mod` 6

sensedCell :: Pos -> Dir -> SenseDir -> Pos
sensedCell p _ Here       = p
sensedCell p d Ahead      = adjacentCell p d
sensedCell p d LeftAhead  = adjacentCell p (turn Left d)
sensedCell p d RightAhead = adjacentCell p (turn Right d)

{- Geography functions -}

rocky :: Pos -> World Bool
rocky p = do c <- cellAt p
             return (isRocky c)

someAntIsAt :: Pos -> World Bool
someAntIsAt p = do c <- cellAt p
                   return (maybe False (const True) (ant c))

antAt :: Pos -> World Ant
antAt p = do c <- cellAt p
             return (maybe (error "call to antAt not expected") id (ant c))

-- TODO: what happens if an ant is already in the cell?
setAntAt :: Pos -> Ant -> World ()
setAntAt p a = do c <- cellAt p
                  b <- getBoard
                  as <- getAnts
                  liftIO $ writeArray b p (c {ant = Just a})
                  liftIO $ writeArray as (antid a) (Just p)

-- | The 'updateAntAt' is similar to the 'setAntAt' function, except that it
--   assumes that it is the 'Ant' that is already at that position that is being
--   updated, and so is able to save some time by no longer having to change the
--   'ants' array in addition to the 'board' array.
updateAntAt :: Pos -> Ant -> World ()
updateAntAt p a = do b <- getBoard
                     c <- cellAt p
                     liftIO $ writeArray b p (c {ant = Just a})

updateAnt :: Ant -> World ()
updateAnt a = do p <- findAnt (antid a)
                 b <- getBoard
                 c <- cellAt p
                 liftIO $ writeArray b p (c {ant = Just a})

clearAntAt :: Pos -> World ()
clearAntAt p = do c <- cellAt p
                  case (ant c) of
                    Nothing -> return ()
                    Just a  -> do b <- getBoard
                                  as <- getAnts
                                  liftIO $ writeArray b p (c {ant = Nothing })
                                  liftIO $ writeArray as (antid a) Nothing

antIsAlive :: AntId -> World Bool
antIsAlive ai = do a <- getAnts
                   b <- liftIO $ readArray a ai
                   return (maybe False (const True) b)

findAnt :: AntId -> World Pos
findAnt ai = do a <- getAnts
                pos <- liftIO $ readArray a ai
                return (maybe (error "call to findAnt not expected") id pos)

killAntAt :: Pos -> World ()
killAntAt = clearAntAt

foodAt :: Pos -> World Int
foodAt p = do c <- cellAt p
              return (food c)

setFoodAt :: Pos -> Int -> World ()
setFoodAt p f = do c <- cellAt p
                   b <- getBoard
                   liftIO $ writeArray b p (c { food = f })

anthillAt :: Pos -> Color -> World Bool
anthillAt p col = do c <- cellAt p
                     return (maybe False (== col) (anthill c))

{- Chemistry functions -}

setMarkerAt :: Pos -> Color -> Marker -> World ()
setMarkerAt p col m = do w <- getBoard
                         liftIO $ do c <- readArray w p
                                     case col of
                                       Red   -> let cur = raMarkers c
                                                in writeArray w p (c {raMarkers = setBit cur m})
                                       Black -> let cur = baMarkers c
                                                in writeArray w p (c {baMarkers = setBit cur m})

clearMarkerAt :: Pos -> Color -> Marker -> World ()
clearMarkerAt p col m = do w <- getBoard
                           liftIO $ do c <- readArray w p
                                       case col of
                                         Red   -> let cur = raMarkers c
                                                  in writeArray w p (c {raMarkers = clearBit cur m})
                                         Black -> let cur = baMarkers c
                                                  in writeArray w p (c {baMarkers = clearBit cur m})

checkMarkerAt :: Pos -> Color -> Marker -> World Bool
checkMarkerAt p col m = do c <- cellAt p
                           case col of
                             Red   -> return (testBit (raMarkers c) m)
                             Black -> return (testBit (baMarkers c) m)

checkAnyMarkerAt :: Pos -> Color -> World Bool
checkAnyMarkerAt p col = do c <- cellAt p
                            case col of
                              Red   -> return (raMarkers c /= 0)
                              Black -> return (baMarkers c /= 0)

{- Phenomenology functions -}

cellMatches :: Pos -> Condition -> Color -> World Bool
cellMatches p cnd col =
  do r <- rocky p
     (if r
      then if cnd == Rock then return True else return False 
      else (case cnd of
              Friend         -> do c1 <- someAntIsAt p
                                   if c1
                                     then do a <- antAt p
                                             return (color a == col)
                                     else return False

              Foe            -> do c1 <- someAntIsAt p
                                   if c1
                                     then do a <- antAt p
                                             return (color a /= col)
                                     else return False

              FriendWithFood -> do c1 <- someAntIsAt p
                                   if c1
                                     then do a <- antAt p
                                             return ((color a == col) && (hasFood a))
                                     else return False

              FoeWithFood    -> do c1 <- someAntIsAt p
                                   if c1
                                     then do a <- antAt p
                                             return ((color a /= col) && (hasFood a))
                                     else return False

              Food           -> do f <- foodAt p
                                   return (f > 0)

              Rock           -> return False

              Marker m       -> checkMarkerAt p col m

              FoeMarker      -> checkAnyMarkerAt p (otherColor col)

              Home           -> anthillAt p col

              FoeHome        -> anthillAt p (otherColor col)))

{- Neurology functions -}

getCode :: Color -> World Code
getCode Red   = getCodes >>= return . fst
getCode Black = getCodes >>= return . maybe (error "No code for Black team!") id . snd

getInstruction :: Color -> AntState -> World Instruction
getInstruction col s = do c <- getCode col
                          return (fst (c ! s))

getComment :: Color -> AntState -> World String
getComment col s = do c <- getCode col
                      return (snd (c ! s))

{- Martial Arts functions -}

adjacentAnts :: Pos -> Color -> World Int
adjacentAnts p col = do res <- mapM (antOfColor . adjacentCell p) [0..5]
                        return (length . filter (== True) $ res)
  where antOfColor pos = do c1 <- someAntIsAt pos
                            if c1
                              then do a <- antAt pos
                                      return (color a == col)
                              else return False

checkForSurroundedAntAt :: Pos -> World ()
checkForSurroundedAntAt p = do c1 <- someAntIsAt p
                               if c1
                                 then do a <- antAt p
                                         aa <- adjacentAnts p (otherColor (color a))
                                         if aa >= 5
                                           then do killAntAt p
                                                   f <- foodAt p
                                                   f' <- if hasFood a then return (f+4) else return (f+3)
                                                   setFoodAt p f'
                                           else return ()
                                  else return ()

checkForSurroundedAnts :: Pos -> World ()
checkForSurroundedAnts p = do let ps = p : (map (adjacentCell p) [0..5])
                              mapM_ checkForSurroundedAntAt ps

{- Number Theory functions -}

_SEED :: Int
_SEED = 12345

mkRng :: Int -> [Int]
mkRng = drop 4
        . map ((`mod` 16384)
        . (`div` 65536))
        . iterate ((`mod` (65536*16384))
        . (1 +)
        . (22695477 *))

randomInt :: Int -> World Int
randomInt n = do
     r:xs <- getRng
     putRng xs
     return $ r `mod` n

{- Kinetics functions -}

step :: AntId -> World ()
step ai = do
    isAlive <- antIsAlive ai
    if isAlive
      then do
        p <- findAnt ai
        a <- antAt p
        if resting a > 0
          then do
            let a' = setResting a (pred $ resting a)
            updateAntAt p a'
          else do
            is <- getInstruction (color a) (state a) 
            case is of
                Sense sensedir st1 st2 cond -> do
                    let p' = sensedCell p (direction a) sensedir
                    m <- cellMatches p' cond (color a)
                    let st = if m then st1 else st2
                    updateAntAt p (setState a st)

                Mark i st                   -> do
                    setMarkerAt p (color a) i
                    updateAntAt p (setState a st)

                Unmark i st                 -> do
                    clearMarkerAt p (color a) i
                    updateAntAt p (setState a st)

                PickUp st1 st2              -> do
                    fd <- foodAt p
                    if (hasFood a) || fd == 0
                      then updateAntAt p (setState a st2)
                      else do
                        setFoodAt p (pred fd)
                        let a' = setHasFood a True
                        updateAntAt p (setState a' st1)

                Drop st                     -> do
                    if hasFood a
                      then do
                        fd <- foodAt p
                        setFoodAt p (succ fd)
                        let a' = setHasFood a False
                        updateAntAt p (setState a' st)
                      else updateAntAt p (setState a st)

                Turn lr st                  -> do
                    let a' = setDirection a $ turn lr (direction a)
                    updateAntAt p (setState a' st)

                Move st1 st2                -> do
                    let newp = adjacentCell p (direction a)
                    is_rocky <- rocky newp
                    is_taken <- someAntIsAt newp
                    if is_rocky || is_taken
                      then updateAntAt p (setState a st2)
                      else do
                        clearAntAt p
                        let a' = setState a st1
                        let a'' = setResting a' 14
                        setAntAt newp a''
                        checkForSurroundedAnts newp

                Flip n st1 st2              -> do
                    num <- randomInt n
                    let st = if num == 0 then st1 else st2
                    updateAntAt p (setState a st)
      else
        return ()

{- Judgement functions -}

foodAtAnthill :: Color -> World Int
foodAtAnthill col = do
    b <- getBoard
    cs <- liftIO $ getElems b
    return $ sum [ food c | c <- cs, anthillOfColor c]
    where
        anthillOfColor cell = maybe False (== col) (anthill cell)
