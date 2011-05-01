{-# OPTIONS -fglasgow-exts #-}
-- | The "VisualizerTools" module defines various utilities used in the
--   'Visualizer'. Basically, they are divided into three large parts:
--
--   * the 'Cell' classifier (see 'classifyCells')
--
--   * the 'Cell' painter (see 'paintCells'\/'paintCells2')
--
--   * the 'Point' to 'Pos'ition transformer (see 'pointToCell')
--
--   There are also some functions for showing some information about 'Cell'
--   contents, as well as the states of the 'Ant's, but these are rather
--   trivial (see 'outputCellInfo').
--
--   The 'Cell' classifier has the purpose of creating a 'Cells' structure which
--   contains all the 'Cell's separated by their types (rocky, clear, etc.). The
--   same 'Cells' structure also includes separate entries for the 'Cell's which
--   contain ants, ants with food or markers. These will be duplicates of the
--   'Cell's in other groups. This classification is required because each of
--   these categories of 'Cell's has to be drawn using a different color. And,
--   without separating them, we wouldn't be able to set the drawing color once
--   per group, but instead we would have to do it for each 'Cell' in
--   particular. This leads to a lot of unnecessary foreign function calls,
--   resulting in large amounts of time necessary for drawing the 'board'.
--
--   In addition to storing the 'Pos'itions of each 'Cell', we also use the
--   'Cells' data structure to store, when necessary, other information which
--   is needed in order to draw the 'Cell' (for example, the value of the
--   markers, the direction of the 'Ant' and so on). Last, but not least,
--   since there's a lot of computation involved in finding out all the 'Point's
--   which represent the corners of the hexagon display of a 'Cell', as well
--   as those which represent the triangle display of an 'Ant', we prefer
--   to compute these values for each 'Cell' only once, on initialization of
--   our program, and make them part of the 'WorldState'. We do this in the
--   "World" module. Thus, in addition to the 'Pos'itions of the 'Cell' and
--   possibly some other necessary information, we also put the 'CacheData'
--   in each entry of the 'Cells' data structure. This 'CacheData' contains
--   the aformentioned hexagon 'Point's and triagle 'Point's for each 'Cell'.
--
--   Once we have this organization of 'Cell's in the 'Cells' data structure we
--   can proceed to painting them. This is done by the 'paintCells' and
--   'paintCells2' functions, which simply draw the 'Cell's from the 'Cells'
--   data structure. Drawing is done in two steps: first, the background is
--   drawn (this will be the same every time, so then we only have to do it
--   once, then we can save it as a bitmap, and draw it faster every other time)
--   and then the contents of the 'Cell' is superimposed on the background. This
--   is achieved with the 'paintCells2' function.
--
--   Finally, in order to be able to display information about a 'Cell', we need
--   to be able to tell which 'Cell' is selected with the mouse. For this, we
--   need to transform screen coordinates (in a 'Point' structure) to a
--   'Pos'ition on the 'board'. This is done by the 'pointToPos' function.
--   However, the interface function is called 'pointToCell' since this one also
--   retrieves the 'Cell' associated with a 'Pos'ition from the 'board' and
--   returns it together with the 'Pos'ition itself. This function can then be
--   called from any type of mouse handler in order to determine the 'Cell'
--   pointed to by the mouse.
--
module VisualizerTools where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Int
import Data.Array.MArray

import World
import Ant
import Data.Bits

-- | Paints the empty 'Cell's using some base color for the background. The idea
--   is that the 'Cell' contents (i.e., 'Ant', marker, food) is not drawn, just
--   the background.
paintCells :: DC a -> WorldState -> Int -> IO ()
paintCells dc ws size = do
  b <- return $ board ws
  c <- return $ cache ws
  cellSet <- classifyCells b c
  cells <- getElems b

  -- draw the rocky cells
  set dc [brushColor := colorRocky, brushKind := BrushSolid]
  mapM_ (drawRocky dc size) (_rocky cellSet)

  -- draw the anthills
  set dc [brushColor := colToCol Red]
  mapM_ (drawAnthill dc size Red) (_redAnthill cellSet)
  set dc [brushColor := colToCol Black]
  mapM_ (drawAnthill dc size Black) (_blackAnthill cellSet)

  -- draw the clear cells
  set dc [brushColor := colorClear]
  mapM_ (drawClear dc size) (_clear cellSet)

  -- draw also the food cells as clear cells (background)
  set dc [brushColor := colorClear]
  mapM_ (drawFood dc size) (_food cellSet)

-- | Paints the contents of 'Cell's (i.e., 'Ant's, markers, food). See also
--   'paintCells'.
paintCells2 :: DC a -> WorldState -> Int -> IO ()
paintCells2 dc ws size = do
  b <- return $ board ws
  c <- return $ cache ws
  cellSet <- classifyCells b c

  -- draw the food cells
  set dc [brushColor := colorFood]
  mapM_ (drawFood dc size) (_food cellSet)

  -- draw the ants
  set dc [brushColor := colToCol2 Red]
  mapM_ (drawAnt dc size) (_redAnt cellSet)
  set dc [brushColor := colToCol2 Black]
  mapM_ (drawAnt dc size) (_blackAnt cellSet)

  -- draw the food on ants
  set dc [brushColor := colorAntFood]
  mapM_ (drawFoodOnAnt dc size) (_foodAnt cellSet)
  
-- | Draws a rocky 'Cell'.
drawRocky :: DC a               -- ^ the device context
        -> Int                  -- ^ the size of the hexagon's side
        -> (Pos, CacheData)     -- ^ the 'Pos'ition of the 'Cell' on the 'Board'
                                --   and the cache of precomputed points
        -> IO ()
drawRocky dc l (p, cd) = polygon dc (hexPoints cd) []

-- | Draws a clear 'Cell'.
drawClear :: DC a               -- ^ the device context
        -> Int                  -- ^ the size of the hexagon's side
        -> (Pos, CacheData)     -- ^ the 'Pos'ition of the 'Cell' on the 'Board'
                                --   and the cache of precomputed points
        -> IO ()
drawClear dc l (p, cd) = polygon dc (hexPoints cd) []

-- | Draws a 'Cell' which is an anthill for the given 'Color'.
drawAnthill :: DC a             -- ^ the device context
        -> Int                  -- ^ the size of the hexagon's side
        -> Ant.Color            -- ^ the 'Color' of the anthill
        -> (Pos, CacheData)     -- ^ the 'Pos'ition of the 'Cell' on the 'Board'
                                --   and the cache of precomputed points
        -> IO ()
drawAnthill dc l col (p, cd) = polygon dc (hexPoints cd) []

-- | Draws an 'Ant' in the 'Cell'.
drawAnt :: DC a                         -- ^ the device context
        -> Int                          -- ^ the size of the hexagon's side
        -> (Ant, Pos, CacheData)        -- ^ the 'Ant'
                                        --   and the 'Pos'ition of the 'Cell' on the 'Board'
                                        --   and the cache of precomputed points
        -> IO ()
drawAnt dc l (a, p, cd) = case (direction a) of
                            0 -> polygon dc (dir0Points cd) []
                            1 -> polygon dc (dir1Points cd) []
                            2 -> polygon dc (dir2Points cd) []
                            3 -> polygon dc (dir3Points cd) []
                            4 -> polygon dc (dir4Points cd) []
                            5 -> polygon dc (dir5Points cd) []

-- | Draws a white circle (supposedly on an 'Ant') to suggest that the 'Ant' is
--   carrying food.
drawFoodOnAnt :: DC a           -- ^ the device context
        -> Int                  -- ^ the size of the hexagon's side
        -> (Pos, CacheData)     -- ^ the 'Pos'ition of the 'Cell' in which to draw the circle
                                --   and the cache of precomputed points
        -> IO ()
drawFoodOnAnt dc l (p, cd) = do (c, r) <- return ( Point
                                                     (Prelude.round (center_x cd))
                                                     (Prelude.round (center_y cd))
                                                 , Prelude.round (fromIntegral l / 4)
                                                 )
                                circle dc c r []
                                
-- | Draws a 'Cell' which contains food.
drawFood :: DC a                        -- ^ the device context
        -> Int                          -- ^ the size of the hexagon's side
        -> (Int, Pos, CacheData)        -- ^ the amount of food in a cell
                                        --   and the 'Pos'ition of the 'Cell' on the 'Board'
        -> IO ()
drawFood dc l (f, p, cd) = do --set dc [brushColor := foodToCol f]
                              polygon dc (hexPoints cd) []

-- | Draws the red markers in a 'Cell'. (current implementation does nothing)
drawRedMarkers :: DC a  -- ^ the device context
        -> Int          -- ^ the size of the hexagon's side
        -> (Int8, Pos, CacheData)
        -> IO ()
drawRedMarkers dc l (i, p, cd) = return ()                                      

-- | Draws the black markers in a 'Cell'. (current implementation does nothing)
drawBlackMarkers :: DC a        -- ^ the device context
        -> Int                  -- ^ the size of the hexagon's side
        -> Pos                  -- ^ the 'Pos'ition of the 'Cell' on the 'Board'
        -> Int8                 -- ^ the markers
        -> IO ()
drawBlackMarkers dc l p m = return ()

colToCol :: Ant.Color -> Graphics.UI.WX.Color
colToCol Red    = colorRedAnthill
colToCol Black  = colorBlackAnthill

colToCol2 :: Ant.Color -> Graphics.UI.WX.Color
colToCol2 Red   = colorRedAnt
colToCol2 Black = colorBlackAnt

foodToCol :: Int -> Graphics.UI.WX.Color
foodToCol n = rgb 50 (255 - n * 20) 50

colorRocky :: Graphics.UI.WX.Color
colorRocky = rgb 184 134 11

colorClear :: Graphics.UI.WX.Color
colorClear = white

colorRedAnthill :: Graphics.UI.WX.Color
colorRedAnthill = rgb 246 136 136

colorBlackAnthill :: Graphics.UI.WX.Color
colorBlackAnthill = grey

colorRedAnt :: Graphics.UI.WX.Color
colorRedAnt = red

colorBlackAnt :: Graphics.UI.WX.Color
colorBlackAnt = black

colorFood :: Graphics.UI.WX.Color
colorFood = rgb 50 180 50

colorLittleFood :: Graphics.UI.WX.Color
colorLittleFood = rgb 50 205 50

colorMuchFood :: Graphics.UI.WX.Color
colorMuchFood = rgb 34 139 34

colorAntFood :: Graphics.UI.WX.Color
colorAntFood = white

colorRedMarker :: Graphics.UI.WX.Color
colorRedMarker = rgb 150 150 150

{-
   The main purpose of the following part is to create a Cells structure
   which contains all the information needed for drawing the board.
-}

-- | A data structure containg various groups of 'Cell's. The groups are
--   _not_ disjoint.
data Cells = Cells { _rocky :: [(Pos, CacheData)]   -- ^ rocky 'Cell's
                   , _clear :: [(Pos, CacheData)]   -- ^ clear 'Cell's
                   , _redAnthill :: [(Pos, CacheData)]    -- ^ 'Red' anthill 'Cell's
                   , _blackAnthill :: [(Pos, CacheData)]  -- ^ 'Black' anthill 'Cell's
                   , _food :: [(Int, Pos, CacheData)]   -- ^ food 'Cell's
                   , _redMarkers :: [(Int8, Pos, CacheData)]  -- ^ 'Cell's with red ant markers
                   , _blackMarkers :: [(Int8, Pos, CacheData)]  -- ^ 'Cell's with black ant markers
                   , _redAnt :: [(Ant, Pos, CacheData)]   -- ^ 'Cell's which contain 'Red' 'Ant's
                   , _blackAnt :: [(Ant, Pos, CacheData)] -- ^ 'Cell's which contain 'Black' 'Ant's
                   , _foodAnt :: [(Pos, CacheData)]   -- ^ 'Cell's which contain 'Ant's (either 'Red' or 'Black') carrying food
                   }

-- | Classifies 'Cell's accoring to their contents in various groups. See the
--   comments at the beginning of this module for more information.
classifyCells :: Board -> Cache -> IO Cells
classifyCells b cache = do ps <- return (indices b)
                           cs <- getElems b
                           cds <- return (elems cache)
                           return (foldr ($) emptyCells (zipWith ($) (zipWith ($) (map f cs) ps) cds))
  where f c pos cd cells = typeCell c pos cd . contentsCell c pos cd $ cells
        emptyCells = Cells [] [] [] [] [] [] [] [] [] []

typeCell :: Cell -> Pos -> CacheData -> Cells -> Cells
typeCell c pos cd cells
  | isRocky c           = cells { _rocky = (pos, cd) : (_rocky cells) }
  | food c > 0          = cells { _food = (food c, pos, cd) : (_food cells) }
  | otherwise           = case (anthill c) of
                            Just Red   -> cells { _redAnthill = (pos, cd) : (_redAnthill cells) }
                            Just Black -> cells { _blackAnthill = (pos, cd) : (_blackAnthill cells) }
                            Nothing    -> cells { _clear = (pos, cd) : (_clear cells) }

contentsCell :: Cell -> Pos -> CacheData -> Cells -> Cells
contentsCell c pos cd cells = addAnt (ant c) cd
                              . addMarkers (raMarkers c) Red cd
                              . addMarkers (baMarkers c) Black cd
                              . addFood (ant c) cd
                              $ cells
  where addAnt Nothing _ cells   = cells
        addAnt (Just a) cd cells = case (Ant.color a) of
                                     Red   -> cells { _redAnt = (a, pos, cd) : (_redAnt cells) }
                                     Black -> cells { _blackAnt = (a, pos, cd) : (_blackAnt cells) }
        addMarkers 0 _ _ cells       = cells
        addMarkers m Red cd cells    = cells { _redMarkers = (m, pos, cd) : (_redMarkers cells) }
        addMarkers m Black cd cells  = cells { _blackMarkers = (m, pos, cd) : (_blackMarkers cells) }
        addFood Nothing _ cells   = cells
        addFood (Just a) cd cells = if (hasFood a)
                                      then cells { _foodAnt = (pos, cd) : (_foodAnt cells) }
                                      else cells

{-
   These functions map the position of a screen point to the position on the board.
-}

-- | Given a 'Point' on the screen, the 'Board' itself and the size of the
--   hexagon's side, this function determines the 'Pos'ition on the 'Board'
--   indicated by the 'Point' and returns it together with the 'Cell' at that
--   'Pos'ition. The return type is a 'Maybe' because the 'Point' could also be
--   outside the 'Board'.
pointToCell :: Point      -- ^ the 'Point' indicating the screen coordinates
  -> Int        -- ^ the size of the hexagon's side
  -> Board      -- ^ the 'Board' of 'Cell's
  -> IO (Maybe (Cell, Pos)) -- ^ the ('Cell', 'Pos') tuple indicated by the 'Point', if any
pointToCell p l b = case pointToPos p l (bounds b) of
                      Nothing  -> return Nothing
                      Just pos -> do cell <- readArray b pos
                                     return (Just (cell, pos))

-- | Given a 'Point' on the screen, the size of the hexagon's side and a range
--   of valid 'Pos'itions, this function returns the 'Pos'ition indicated by
--   the 'Point'. The return type is a 'Maybe' because the 'Point' could also
--   indicate a 'Pos'ition outside the range.
pointToPos :: Point     -- ^ the 'Point' indicating the screen coordinates
  -> Int        -- ^ the size of the hexagon's side
  -> (Pos, Pos)     -- ^ the range of valid 'Pos'itions
  -> Maybe Pos      -- ^ the 'Pos'ition indicated by 'Point', if in range
pointToPos (Point x y) l range = if (inRange range p) then Just p else Nothing
  where yy      = (2 * fromIntegral y) / (3 * ll)
        ll      = fromIntegral l
        p       = getPosition
        getPosition
          | (yy - fromIntegral (floor yy)) >= 1/3       = getBasicPosition x (floor yy) l
          | otherwise                                   = getExtPosition (x, y) (floor yy) l

getBasicPosition :: Int -> Int -> Int -> Pos
getBasicPosition x k l
  | k `mod` 2 == 0 = Pos (floor (fromIntegral x / (2 * x_length))) k
  | otherwise      = Pos (floor ((fromIntegral x - x_length) / (2 * x_length))) k
  where x_length = sin (pi / 3.0) * fromIntegral l

getExtPosition :: (Int, Int) -> Int -> Int -> Pos
getExtPosition (x, y) k l
  | (k2 == 0) && (xl2 == 0)     = getPosCase1 (x, y) xl x_length k l
  | (k2 == 0) && (xl2 == 1)     = getPosCase2 (x, y) xl x_length k l
  | (k2 == 1) && (xl2 == 0)     = getPosCase3 (x, y) xl x_length k l
  | (k2 == 1) && (xl2 == 1)     = getPosCase4 (x, y) xl x_length k l
  where x_length = sin (pi / 3.0) * fromIntegral l
        k2       = k `mod` 2
        xl       = floor (fromIntegral x / x_length)
        xl2      = xl `mod` 2

getPosCase1 (x, y) xl x_length k l
  | sameSign (f rx ry) (f (fromIntegral x) (fromIntegral y))    = Pos (xl `div` 2) k
  | otherwise                                                   = Pos ((xl `div` 2) - 1) (k - 1)
  where (x1, y1) = (fromIntegral xl * x_length, fromIntegral ((3 * k + 1) * l) / 2)
        (x2, y2) = (fromIntegral (xl + 1) * x_length, fromIntegral (3 * k * l) / 2)
        (rx, ry) = (fromIntegral (xl + 1) * x_length, fromIntegral ((3 * k + 1) * l) / 2)
        f        = lineEq (x1, y1) (x2, y2)

getPosCase2 (x, y) xl x_length k l
  | sameSign (f rx ry) (f (fromIntegral x) (fromIntegral y))    = Pos (xl `div` 2) k
  | otherwise                                                   = Pos (xl `div` 2) (k - 1)
  where (x1, y1) = (fromIntegral (xl + 1) * x_length, fromIntegral ((3 * k + 1) * l) / 2)
        (x2, y2) = (fromIntegral xl * x_length, fromIntegral (3 * k * l) / 2)
        (rx, ry) = (fromIntegral xl * x_length, fromIntegral ((3 * k + 1) * l) / 2)
        f        = lineEq (x1, y1) (x2, y2)

getPosCase3 (x, y) xl x_length k l
  | sameSign (f rx ry) (f (fromIntegral x) (fromIntegral y))    = Pos ((xl `div` 2) - 1) k
  | otherwise                                                   = Pos (xl `div` 2) (k - 1)
  where (x1, y1) = (fromIntegral (xl + 1) * x_length, fromIntegral ((3 * k + 1) * l) / 2)
        (x2, y2) = (fromIntegral xl * x_length, fromIntegral (3 * k * l) / 2)
        (rx, ry) = (fromIntegral xl * x_length, fromIntegral ((3 * k + 1) * l) / 2)
        f        = lineEq (x1, y1) (x2, y2)

getPosCase4 (x, y) xl x_length k l
  | sameSign (f rx ry) (f (fromIntegral x) (fromIntegral y))    = Pos (xl `div` 2) k
  | otherwise                                                   = Pos (xl `div` 2) (k - 1)
  where (x1, y1) = (fromIntegral xl * x_length, fromIntegral ((3 * k + 1) * l) / 2)
        (x2, y2) = (fromIntegral (xl + 1) * x_length, fromIntegral (3 * k * l) / 2)
        (rx, ry) = (fromIntegral (xl + 1) * x_length, fromIntegral ((3 * k + 1) * l) / 2)
        f        = lineEq (x1, y1) (x2, y2)

sameSign x y = x * y > 0

lineEq (x1, y1) (x2, y2) x y = y * (x2 - x1) + x * (y1 - y2) + x1 * y2 - x2 * y1

{-
   These functions output some information about the cells and about the state
   of the ants.
-}

-- | Displays information about the contents of a 'Cell' in a 'String' form.
outputCellInfo :: Cell -> Pos -> Codes -> String
outputCellInfo c@(Cell r a ah f rm bm) p codes =
    "At position " ++ (show p) ++ ": " ++ (show c) ++ (showStateInfo a codes)

showStateInfo :: Maybe Ant -> Codes -> String
showStateInfo Nothing _                                 = ""
showStateInfo (Just (Ant _ Red s _ _ _)) (rc, _)        = {-", instruction: " ++ (show (fst (rc ! s))) ++-}
                                                          showComment (snd (rc ! s))
showStateInfo (Just (Ant _ Black s _ _ _)) (_, Just bc) = {-", instruction: " ++ (show (fst (bc ! s))) ++-}
                                                          showComment (snd (bc ! s))

showComment :: String -> String
showComment ""  = ""
showComment com = ", state comment: " ++ com
