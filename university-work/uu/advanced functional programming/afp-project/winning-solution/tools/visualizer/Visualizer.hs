module Main where

import Interface
import VisualizerState hiding (on)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Monad hiding (when)
import qualified Control.Monad as C.M
import Data.FiniteMap
import Data.Set
import Data.List
import Data.IORef
import Data.Maybe
import System
import Debug.QuickCheck

-- boardsize = 100
inithex     = 6

(offx,offy) = (20,20)

-- half the x-length
hexx :: Int -> Int
hexx = \hexsize -> round (cos (pi/6.0) * fromIntegral hexsize)

-- change a position into a screen coordinate (center of hexagon)
pos2coord :: Int -> Pos -> Point
pos2coord hexsize (Pos x y) = 
                 pt ((2*x - (y+1) `mod` 2) * hexx hexsize + offx) 
                    (hexsize + y * (hexsize + (hexx hexsize `div` 2)) + offy)


-- get all positions of all fields in a list
allpos :: Int -> Int -> [Pos]
allpos = \hexsize ->
  \s -> [ (Pos x y) | y <- [0..s-1], x <- [0..s-1], onn s (Pos x y) ]

-- corner points of a hexagon
hexpoints :: Int -> Pos -> [Point]
hexpoints hexsize p@(Pos x y) = 
    [ c { pointY = pointY c - hexsize } -- 12 o'clock
    , c { pointX = pointX c + hexx', pointY = pointY c - hexx' `div` 2 } 
                                        -- 2 o'clock
    , c { pointX = pointX c + hexx', pointY = pointY c + hexx' `div` 2 } 
                                        -- 4 o'clock
    , c { pointY = pointY c + hexsize } -- 6 Uhr
    , c { pointX = pointX c - hexx', pointY = pointY c + hexx' `div` 2 } 
                                        -- 8 o'clock
    , c { pointX = pointX c - hexx', pointY = pointY c - hexx' `div` 2 } 
                                        -- 10 o'clock
    ]
  where c = pos2coord hexsize p
        hexx' = hexx hexsize


-- returns side plus corner to the left
dir2sidepoint :: Int -> Pos -> Maybe Dir -> (Point,Point)
dir2sidepoint hexsize p@(Pos x y) Nothing = (pos2coord hexsize p,pos2coord hexsize p)
dir2sidepoint hexsize p@(Pos x y) (Just dir) = 
  let hps = hexpoints hexsize p
      hps2 = drop 1 (hps ++ hps) -- because hexpoints starts with 12,
                                 -- but direction 0 points to the right,
                                 -- we drop one
      [p1,p2] = take 2 (drop dir hps2) -- relevant corners
  in  (pt ((pointX p1 + pointX p2) `div` 2)
          ((pointY p1 + pointY p2) `div` 2)
      ,p1)


-- transforms coordinates back into a position
coord2pos :: Int -> Int -> Point -> Pos
coord2pos hexsize s cr = 
                 (\(x,y) -> Pos x y)
               $ fst
               $ minimumBy (\x y -> compare (snd x) (snd y))
               $ map (\(p,c) -> (p,dist cr c))
               $ cs
  where minc = pos2coord hexsize (Pos 0 0)
        maxc = pos2coord hexsize (Pos (s-1) (s-1))
        minx = pointX minc
        miny = pointY minc
        maxx = pointX maxc
        maxy = pointY maxc
        ux = fromIntegral (maxx - minx) / fromIntegral s :: Double
        uy = fromIntegral (maxy - miny) / fromIntegral s :: Double
        cx = round $ fromIntegral (pointX cr - minx) / ux
        cy = round $ fromIntegral (pointY cr - miny) / uy
        cs = [ ((x,y),pos2coord hexsize (Pos x y)) 
             | x <- 0:[(cx - 3)..(cx + 3)] :: [X]
             , y <- 0:[(cy - 3)..(cy + 3)] :: [Y]
             , onn s (Pos x y) ]



boarddata :: Gen (Int,Int,Int,Int)
boarddata = do boardsize <- choose (10,200)
               hexsize <- choose (2,6)
               x <- choose (0,boardsize)
               y <- choose (0,boardsize)
               return (boardsize,hexsize,x,y)


-- prop_coord_pos_inverses :: Int -> Int -> Int -> Int -> Property
prop_coord_pos_inverses = forAll boarddata $ \(boardsize,hexsize,x,y) ->
     let pos = Pos x y 
     in pos == coord2pos hexsize boardsize (pos2coord hexsize pos)

dist :: Point -> Point -> Double
dist p1 p2 = sqrt (fromIntegral $
                     abs (pointX p1 - pointX p2) ^ 2 
                   + abs (pointY p1 - pointY p2) ^ 2)



data Redraw = Completely | OnlyFields (Set Pos)


-- this one contains all the initializations and the event handlers
go :: Maybe String -> Bool -> IO ()
go ifile idebug
   = do  s         <-  varCreate emptyBoard
         hexsizer  <-  varCreate inithex         -- initial size setting
         hexsize   <-  varGet hexsizer
         cmds      <-  varCreate []              -- current program
         redraw    <-  varCreate (OnlyFields emptySet) 
                                                 -- redraw parts of screen

	 openFile ifile cmds

         mdc       <-  memoryDCCreate

         let n   =  boardsize emptyBoard
         -- don't use hexx due to rounding ...
         let wx  =  pointX (pos2coord hexsize (Pos (n-1) (n-1))) + 2 * offx
         let wy  =  pointY (pos2coord hexsize (Pos (n-1) (n-1))) + 2 * offy
     
         f         <-  frame [ text := "Visualizer" ]
         let status = f

         p         <-  scrolledWindow f []

         ps        <-  panel f []
         hex       <-  textCtrl ps WrapWord [text := show inithex]
         step      <-  button ps [ text := "->"
                                 , on command := do performStep redraw p s cmds 1
                                                    showStatus status hexsize s
                                 ]
{-
         step15    <-  button ps [ text := "15 ->"
                                 , on command := performStep15 p s cmds ]
-}
         refresh   <-  button ps [ text := "Refresh"
                                 , on command := fullRefresh redraw hex hexsizer p ]
         gotonr    <-  textCtrl ps WrapWord []
         gotob     <-  button ps [ text := "Skip n"
                                 , on command := skip redraw hex hexsizer gotonr p s cmds
                                 ]


         -- create a timer
         t <- timer f [ interval := 90
                      , on command := do performStep redraw p s cmds 3
                                         showStatus status hexsize s
                      , enabled := False ]

         tdelay    <-  textCtrl ps WrapWord 
                                 [ text := "90" ]

         ttimer    <-  button ps [ text := "Toggle timer"
                                 , on command := updateDelay t tdelay >> 
                                                 set t [ enabled :~ not ] ]


         blackmarks <- button ps [ text := "Black marks"
		                 , on command := do modifyIORef s 
                                                      (\st -> st { showblackmarks 
                                                         = not (showblackmarks st) })
                                                    fullRefresh redraw hex hexsizer p ]

         redmarks <- button ps [ text := "Red marks"
	                       , on command := do modifyIORef s 
                                                      (\st -> st { showredmarks
                                                         = not (showredmarks st) })
                                                  fullRefresh redraw hex hexsizer p ]

         blackants <- button ps [ text := "Black ants"
		                 , on command := do modifyIORef s 
                                                      (\st -> st { showblackants 
                                                         = not (showblackants st) })
                                                    fullRefresh redraw hex hexsizer p ]

         redants <- button ps [ text := "Red ants"
	                      , on command := do modifyIORef s 
                                                    (\st -> st { showredants
                                                         = not (showredants st) })
                                                 fullRefresh redraw hex hexsizer p ]

         set ps [ layout := margin 5 $ column 5 $ [ hfill $ minsize (sz 0 20)
                                                          $ widget hex
                                                  , static $ widget step 
--                                                  , static $ widget step15
                                                  , static $ widget refresh
                                                  , hfill $ minsize (sz 0 20)
                                                          $ widget tdelay
                                                  , static $ widget ttimer 
                                                  , hfill $ minsize (sz 0 20) 
                                                          $ widget gotonr 
                                                  , static $ widget gotob 
						  , static $ widget blackmarks
						  , static $ widget redmarks
						  , static $ widget blackants
						  , static $ widget redants
						  ] ]

         set p [ virtualSize := sz 1600 1200, scrollRate := sz 10 10 ]

         -- trivial layout:
         set f [ layout := row 0 [ fill $ minsize (sz wx wy) $ widget p
                                 , vfill $ widget ps ] ]

         fm        <-  menuPane [ text := "&File" ]

         mOpen     <-  menuItem fm [ text := "Open Dump" ]
         mRefresh  <-  menuItem fm [ text := "Refresh\tCtrl+R" ]
         mStep     <-  menuItem fm [ text := "Step\tCtrl+P" ]
         mTimer    <-  menuItem fm [ text := "Toggle timer\tCtrl+T" ]
         

         set f [ menubar    :=  [fm]
               , fullRepaintOnResize := False
               , on (menu mOpen)     := onOpen f s cmds
               , on (menu mRefresh)  := fullRefresh redraw hex hexsizer p
               , on (menu mStep)     := do performStep redraw p s cmds 1
                                           showStatus status hexsize s
               , on (menu mTimer)    := set t [ enabled :~ not ]
               ]

         set p [ fullRepaintOnResize := False ]

         windowOnPaintRaw p (onPaint p mdc fm redraw s hexsizer cmds)
         windowOnMouseClickMove p (const $ return ())
                                  (onMouseMove status n hexsizer s)

         -- on screen!
         windowShow  f
         windowRaise f

 where
    updateDelay t tdelay =
       do
         delay <- get tdelay text
         case reads delay of
           [(n,"")] -> do  debug ("Setting timer delay to " ++ show n)
                           set t [ interval := n ]

    debug | idebug    = putStrLn
          | otherwise = \x -> return ()

    

    onMouseMove status n hexsizer s c =
       do
         state <- readIORef s
         hexsize <- varGet hexsizer
	 let pos = coord2pos hexsize n c
         modifyIORef s (\st -> st { mousepos = pos })
         doShowStatus status hexsize state pos

    showStatus status hexsize s =
       do state <- readIORef s
          doShowStatus status hexsize state (mousepos state)

    doShowStatus status hexsize state pos =
       do
         let (Pos x y) = pos
	 let statustext = show (step state) ++ ": " ++ show (x,y)
	 let sfood=" food: " ++ show (lookupWithDefaultFM (food state) (-1) pos)
	 let ms=lookupWithDefaultFM (marks state) (replicate 12 False) pos
         let makeMarksText ms' = map (\b -> if b then '1' else '0') ms'
	 let markstring = " red marks: " ++ makeMarksText (reverse (take 6 ms))
                     ++ " black marks: " ++ makeMarksText (reverse (drop 6 ms))
         let statustext' = statustext ++ markstring ++ sfood
         let statustext'' = 
              case lookupFM (ants state) pos of
                Nothing -> id
                Just (_,_,_,rest,antid) -> (++(" ant "++show antid)) .
                                           (if rest == 0 then id else (++ " [rest=" ++ show rest ++ "]")) .
                                           (case lookupFM (comments state) antid of
                                             Nothing -> id
                                             Just comment 
                                                 -> (++(" ("++comment++")")))

         set status [ text := statustext'' statustext' ]

{-
    performStep15 p s cmds =
       do
         sequence_ (replicate 15 (performStep p s cmds False))
-}

    isVStep (VStep _) = True
    isVStep _ = False

    performStep redraw p s cmds dorepaint =
       do
         cmds' <- varGet cmds
         let (ncmds,stcmd:rcmds) = break (isVStep) cmds'
         s' <- varGet s
         let count = step s' 
         debug $ "performing step " ++ show count ++ " for real."
         -- print (take 10 ncmds)
         positions <- mapM (performCmd redraw s) (ncmds ++ [stcmd])
         let positions' = map fromJust $ filter isJust positions
         varUpdate redraw
                   (\x -> case x of
                            Completely -> Completely
                            OnlyFields s -> 
                              OnlyFields $ Data.Set.union s (mkSet positions')
                   )
         varSet cmds rcmds
         when (dorepaint > 0 && count `mod` dorepaint == 0) $ do
           debug "repainting."
           repaint p

    skip redraw hex hexsizer gotonr p s cmds = 
       do
         nr <- get gotonr text
         let nr' = case reads nr of
                     [(n,"")] -> n
                     _        -> 0
         sequence_ (replicate nr' (performStep redraw p s cmds 0))
         fullRefresh redraw hex hexsizer p

    fullRefresh redraw hex hexsizer p =
       do
         varSet redraw Completely
         h <- get hex text
         let h' = case reads h of
                    [(n,"")] -> n
                    _        -> inithex
         varSet hexsizer h'
         repaint p

    onOpen f s cmds =
       do
         fd <- fileDialogCreate f 
                 "Specify dump to load"
                 ""
                 ""
                 "Dump (.dump)|*.dump"
                 pointNull
                 (wxOPEN + wxOVERWRITE_PROMPT)
         res <- dialogShowModal fd
         when (res == wxID_OK) $
              do
                pt <- fileDialogGetPath fd
                openFile (Just pt) cmds

    openFile pt cmds =
       do
                contents <- case pt of
		  Nothing   -> getContents
		  (Just pt) -> readFile pt
                varSet cmds (map read (lines contents))

    onPaint p mdc fm redraw s hexsizer cmds dc newrect@(Rect x y wx wy) _ =
       do
         debug "repaint executed."
         redraw' <- varGet redraw
         let (complete,fields) = case redraw' of
                                   Completely    -> (True,[])
                                   OnlyFields xs -> (False,setToList xs)
         varSet redraw (OnlyFields emptySet)
         s' <- varGet s
         let n = boardsize s'
         hexsize <- varGet hexsizer

         when complete $ do
           -- calculate size of hexagons, based on size of rectangle
           -- the following two equations must hold:
           -- n * 2 * hexx <= wx - 2 * offx
           -- n * (3/2) * hexsize + (1/2) * hexsize <= wy - 2 * offy
           let needed_x = n * 2 * hexx hexsize  + 2 * offx
           let needed_y = (n * 3 * hexsize + 3 * hexsize) `div` 2 + 2 * offy
           set p [ virtualSize := sz needed_x needed_y ]

{-
           let hexsize 
                  = 5 +
                    min 
                   (round ((wx' - 2 * offx') / (2 * n' * cos (pi/6))))
                   (round ((wy' - 2 * offy') / (3/2 * n' + 1/2)))
                where
                 wx'    =  fromIntegral wx
                 wy'    =  fromIntegral wy
                 offx'  =  fromIntegral offx
                 offy'  =  fromIntegral offy
                 n'     =  fromIntegral n
-}
 
           let  pos2coord'  =  pos2coord  hexsize
           let  allpos'     =  allpos    hexsize
           let  hexpoints'  =  hexpoints  hexsize

           varSet hexsizer hexsize

           -- draw all hexagons
           debug "drawing hexagons."
           let acs = map (hexpoints') 
                         (allpos' n)
           mapM_ (\ps -> drawPoly dc ps) acs
           -- draw current state
           debug "drawing state."
           mapM_ (\ps -> drawPos dc hexsize s' ps) (allpos' n)

         -- update positions that want update
         debug "drawing updates."
         mapM_ (\ps -> drawPos dc hexsize s' ps) fields
         return ()

drawPoly :: DC d -> [Point] -> IO ()
drawPoly dc (p : ps) = drawPoly' dc p (ps ++ [p])

drawPoly' dc cp []       = return ()
drawPoly' dc cp (p : ps) = dcDrawLine dc cp p >> drawPoly' dc p ps

{-
drawCmd :: DC d -> Int -> Var State -> VCommand -> IO ()
drawCmd dc hexsize s cmd
                 = do s' <- performCmd s cmd
                      s' <- varGet s
                      case getPos cmd of 
                        Just pos -> drawPos dc hexsize s' pos
                        Nothing  -> return ()
-}

performCmd :: Var Redraw -> Var State -> VCommand -> IO (Maybe Pos)
performCmd redraw s cmd = do varUpdate s $! procCmd cmd
                             return $ getPos cmd

drawPos :: DC d -> Int -> State -> Pos -> IO ()
drawPos dc hexsize s pos 
                    = do 
                         -- as a hack, draw food zero first everywhere, later the real amount,
                         -- if unequal to zero; this will effectively put white hexes everywhere
                         case lookupFM (food s) pos of
                           Nothing   -> return ()
                           Just amount
                                     -> drawFood dc hexsize 0 pos
                         case lookupFM (rocks s) pos of
                           Nothing   -> return ()
                           Just rock -> drawRock dc hexsize pos
                         case lookupFM (anthills s) pos of
                           Nothing   -> return ()
                           Just c    -> drawAnthill dc hexsize c pos
                         case lookupFM (food s) pos of
                           Nothing   -> return ()
                           Just amount | amount > 0
                                     -> drawFood dc hexsize amount pos
                           _         -> return ()
                         case lookupFM (marks s) pos of
                           Nothing   -> return ()
                           Just mark | or mark   -> drawMark dc hexsize mark s pos
                                     | otherwise -> return ()
                         case lookupFM (ants s) pos of
                           Nothing   -> return ()
                           Just (hf,c,dir,rest,antid)
                                     -> drawAnt dc hexsize hf c dir s pos
                        
drawRock :: DC d -> Int -> Pos -> IO ()
drawRock dc hexsize pos = do dcWithBrushStyle dc markbrush $
                               drawPolygon dc (hexpoints hexsize pos)

drawAnthill :: DC d -> Int -> Colour -> Pos -> IO ()
drawAnthill dc hexsize c pos = dcWithBrushStyle dc (hillbrush c) $
                               drawPolygon dc (hexpoints hexsize pos)

drawFood :: DC d -> Int -> Amount -> Pos -> IO ()
drawFood dc hexsize amount pos = dcWithBrushStyle dc (foodbrush amount) $
                                 drawPolygon dc (hexpoints hexsize pos)

drawMark :: DC d -> Int -> Mark -> State -> Pos -> IO ()
drawMark dc hexsize mark s pos = 
    do
      let homered    =  getHomeDir Red mark 
          (hrs,_)    =  dir2sidepoint hexsize pos homered
          foodred    =  getFoodDir Red mark
          (frs,frc)  =  dir2sidepoint hexsize pos foodred
          homeblack  =  getHomeDir Black mark
          (hbs,_)    =  dir2sidepoint hexsize pos homeblack
          foodblack  =  getFoodDir Black mark
          (fbs,fbc)  =  dir2sidepoint hexsize pos foodblack
          center     =  pos2coord hexsize pos
      C.M.when (showredmarks s)
           $ do pen <- markpen Red
                dcWithPen dc pen $ drawPolygon dc [center,frs,frc,center,hrs]
      C.M.when (showblackmarks s) 
	   $ do pen <- markpen Black
		dcWithPen dc pen $ drawPolygon dc [center,fbs,fbc,center,hbs]
  where 
        markpen :: Colour -> IO (Pen ())
        markpen c = penCreateFromColour (col2col c) 2 0


drawAnt :: DC d -> Int -> HasFood -> Colour -> Dir -> State -> Pos -> IO ()
drawAnt dc hexsize hf c dir s pos
 = C.M.when (case c of { Red -> showredants s ; Black -> showblackants s }) $
    do
      -- print pos
      let cs = map
               ( (\(x,y) -> pt (round x) (round y))
               . (+.. ((\p -> ((fromIntegral $ pointX p
                               ,fromIntegral $ pointY p)))
                         (pos2coord hexsize pos)))
               . ((fromIntegral hexsize/2) *..)
               )
               (btriangle (dir2angle dir))
      pen <- antpen hf c
      dcWithPen dc pen $ 
        dcWithBrushStyle dc (antbrush hf c) $
        drawPolygon dc cs
      when hf $ do
        pen <- foodpen
        dcWithPen dc pen $
          dcWithBrushStyle dc foodbrush $
          drawPolygon dc [pos2coord hexsize pos]
  where btriangle a =
          [ (sin a,-cos a)
          , (sin (5*pi/6 + a),-cos (5*pi/6 + a))
          , (sin (7*pi/6 + a),-cos (7*pi/6 + a))
          ]

        antpen :: HasFood -> Colour -> IO (Pen ())
        antpen hf c = penCreateFromColour (col2col' hf c) 3 0

        antbrush :: HasFood -> Colour -> BrushStyle
        antbrush hf c = brushSolid (col2col' hf c)

        foodpen = penCreateFromColour green 5 0
        foodbrush = brushSolid green

dir2angle :: Dir -> Double
dir2angle 5  = pi / 6
dir2angle 0  = pi / 2
dir2angle 1  = 5 * pi / 6
dir2angle 2  = 7 * pi / 6
dir2angle 3  = 3 * pi / 2
dir2angle 4  = 11 * pi / 6


hillpen :: Colour -> IO (Pen ())
hillpen c = penCreateFromColour (col2col c) 0 0

col2col' True Red = yellow
col2col' True Black = blue
col2col' False Red   = red
col2col' False Black = black

col2col = col2col' False

hillbrush :: Colour -> BrushStyle
hillbrush c = brushSolid (brighter 0.75 (col2col c))

foodbrush :: Amount -> BrushStyle
foodbrush amount = brushSolid (brighter (scale amount) green)
  where scale 0 = 1.0
        scale 1 = 0.8
        scale 2 = 0.6
        scale 3 = 0.4
        scale 4 = 0.2
        scale _ = 0

markbrush :: BrushStyle
markbrush = brushSolid rockbrown

whitebrush :: BrushStyle
whitebrush = brushSolid white

rockbrown   = colorRGB  80 0 0

brighter :: Float -> Color -> Color
brighter f c = colorRGB r' g' b'
  where
    h cc = round (f * fromIntegral (255 - cc c)) + cc c
    r' = h colorRed
    g' = h colorGreen
    b' = h colorBlue

windowOnMouseClick :: Window a -> (Either Point Point -> IO ()) -> IO ()
windowOnMouseClick window eventHandler
  = windowOnMouse window False
      (\mouse -> case mouse of
          MouseLeftUp p _  -> eventHandler (Left p)
          MouseRightUp p _ -> eventHandler (Right p)
          _                -> skipCurrentEvent)

windowOnMouseClickMove :: Window a
                       -> (Either Point Point -> IO())  -- handler fuer Maus-Klicks
                       -> (Point -> IO())               -- handler fuer Maus-Bewegung
                       -> IO ()
windowOnMouseClickMove window eventHandlerClick eventHandlerMove
  = windowOnMouse window True
      (\mouse -> case mouse of
          MouseLeftUp p _  -> eventHandlerClick (Left p)
          MouseRightUp p _ -> eventHandlerClick (Right p)
          MouseMotion p _  -> eventHandlerMove p
          _                -> skipCurrentEvent)


main = do args <- System.getArgs
          let idebug  = any (== "-d") args
          let noflags = filter (\x -> not $ "-" `isPrefixOf` x) args
          let ifile = if null noflags then Nothing else Just (head noflags)
          run $ go ifile idebug
