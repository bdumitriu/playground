
module Run where

import Control.Monad.State
import Control.Monad
import Dump (dump)
import Prelude hiding (Either(..))
import Data.Maybe
import Types

type M a = State St a

data St = St {
        st_max_ant :: !AntID,
        st_map :: !Map,
        st_ants :: !Ants,
        st_commands_red :: !Commands,
        st_commands_black :: !Commands,
        st_randoms :: ![Int],
        st_redscore :: !Score,
        st_blackscore :: !Score,
        st_redpickups :: !Pickups,
        st_blackpickups :: !Pickups
    }

mk_rng :: Int -> [Int]
mk_rng = drop 4
       . map ((`mod` 16384) . (`div` 65536))
       . iterate ((`mod` (65536*16384)) . (1 +) . (22695477 *))

run :: Int -> Int -> Int -> Int -> Map -> Ants -> Commands -> Commands
    -> ([VCommand], String)
run start_at iters seed max_ant_id m a cs1 cs2
 = case evalState (do_rounds start_at (start_at + iters)) init_state of
       (cs, s) ->
           (cs, "random seed: " ++ show seed ++ "\n\n" ++ s)
    where init_state = St max_ant_id m a cs1 cs2 (mk_rng seed) 0 0 0 0

do_rounds :: Int -> Int -> M ([VCommand], String)
do_rounds start_at iters
 = do -- error $ "Doing total of " ++ show iters ++ " rounds; " ++ show (start_at, iters)
      res <- mapM (do_round start_at) [0..iters - 1]
      let (css, ss) = unzip res
      return (concat css, concat ss)

do_round :: Int -> Int -> M ([VCommand], String)
do_round start_at i
           = do st <- get
                res <- liftM ((VStep i:) . concat) $ mapM do_step [0..st_max_ant st]
                let res' = if i `mod` 100 == 0
                             then VProfile {
                                    profileRedScore   = st_redscore st,
                                        profileBlackScore = st_blackscore st,
                                            profileRedPickups = st_redpickups st,
                                            profileBlackPickups = st_blackpickups st
                                  } : res
                                 else res
                    res'' = if i < start_at
                            then []
                            else if i == start_at
                            then dump_map (st_map st) (st_ants st) ++ res'
                            else res'
                return (res'', dump (st_map st) (st_ants st) i)

do_step :: AntID -> M [VCommand]
do_step aid =
 do st <- get
    let
        Ants a = st_ants st
        Map m = st_map st
    case lookupFM a aid of
        Nothing -> return []
        Just ant ->
            if a_rest ant > 0
             then do let ant' = ant { a_rest = a_rest ant - 1 }
                     put $! st { st_ants = Ants $ addToFM a aid ant' }
                     return [VAnt (a_food ant) (a_col ant) (a_pos ant)
                                  (a_dir ant) (a_rest ant - 1) aid]
             else let my_pos = a_pos ant
                      my_dir = a_dir ant
                      my_col = a_col ant
                      my_cell = what_at m my_pos
                      Commands cs = case a_col ant of
                                        Red -> st_commands_red st
                                        Black -> st_commands_black st
                      red_marks i b = do let ms = c_redmarks my_cell
                                             ms' = set_marker ms i b
                                             my_cell' = my_cell { c_redmarks = ms' }
                                             m' = update_at m my_pos my_cell'
                                         put $! st { st_map = Map m' }
                                         return []
                      black_marks i b = do let ms = c_blackmarks my_cell
                                               ms' = set_marker ms i b
                                               my_cell' = my_cell { c_blackmarks = ms' }
                                               m' = update_at m my_pos my_cell'
                                           put $! st { st_map = Map m' }
                                           return []
                      C (command, comment) = cs ! a_state ant
                      add_comment xs = if null comment
                                       then xs
                                       else liftM (VComment aid my_pos comment:) xs
                  in add_comment
                   $ case command of
                         Sense dir l1 l2 cond -> {-# SCC "X8" #-}
                             do let pos' = pos_of my_pos my_dir dir
                                matches <- cell_matches pos' cond my_col
                                let state' = if matches then l1 else l2
                                set_state aid state'
                                return []
                         Mark i l -> {-# SCC "X7" #-}
                             do
                                case my_col of
                                    Red -> red_marks i True
                                    Black -> black_marks i True
                                set_state aid l
                                return [VMark my_col my_pos i]
                         Unmark i l -> {-# SCC "X6" #-}
                             do
                                case my_col of
                                    Red -> red_marks i False
                                    Black -> black_marks i False
                                set_state aid l
                                return [VUnMark my_col my_pos i]
                         PickUp l1 l2 -> {-# SCC "X5" #-}
                             if a_food ant || c_food my_cell == 0
                             then do set_state aid l2
                                     return []
                             else do let food' = c_food my_cell - 1
                                         my_cell' = my_cell { c_food = food' }
                                         m' = update_at m my_pos my_cell'
                                         ant' = ant { a_food = True }
                                         a' = addToFM a aid ant'
                                         redscore = if c_type my_cell == Anthill Red then st_redscore st - 1 else st_redscore st
                                         blackscore = if c_type my_cell == Anthill Black then st_blackscore st - 1 else st_blackscore st
                                         redpickups = if my_col == Red then st_redpickups st + 1 else st_redpickups st
                                         blackpickups = if my_col == Black then st_blackpickups st + 1 else st_blackpickups st
                                     put $! st { st_map = Map m',
                                                 st_ants = Ants a',
                                                 st_redscore = redscore,
                                                 st_blackscore = blackscore,
                                                 st_redpickups = redpickups,
                                                 st_blackpickups = blackpickups
                                               }
                                     set_state aid l1
                                     return [VFood my_pos food',
                                             VNoAnt my_pos,
                                             VAnt True my_col my_pos
                                                  (a_dir ant) 0 aid,
                                             VScore redscore  blackscore]
                         Drop l -> {-# SCC "X4" #-}
                             do
                                if (a_food ant)
                                 then
                                     do let food' = c_food my_cell + 1
                                            my_cell' = my_cell { c_food = food' }
                                            m' = update_at m my_pos my_cell'
                                            ant' = ant { a_food = False }
                                            a' = addToFM a aid ant'
                                            redscore = if c_type my_cell == Anthill Red then st_redscore st + 1 else st_redscore st
                                            blackscore = if c_type my_cell == Anthill Black then st_blackscore st + 1 else st_blackscore st
                                        put $! st { st_map = Map m',
                                                    st_ants = Ants a',
                                                    st_redscore = redscore,
                                                    st_blackscore = blackscore
                                                  }
                                        set_state aid l
                                        return [VFood my_pos food',
                                                VNoAnt my_pos,
                                                VAnt False my_col my_pos
                                                     (a_dir ant) 0 aid,
                                                VScore redscore  blackscore]
                                 else do
                                        set_state aid l
                                        return []
                         Turn dir l -> {-# SCC "X3" #-}
                             do
                                let f = case dir of
                                            Left -> left_of
                                            Right -> right_of
                                    ant' = ant { a_dir = f (a_dir ant) }
                                    a' = addToFM a aid ant'
                                put $! st { st_ants = Ants a' }
                                set_state aid l
                                return [VNoAnt my_pos,
                                        VAnt (a_food ant) my_col my_pos
                                             (a_dir ant') 0 aid]
                         Move l1 l2 -> {-# SCC "X2" #-}
                             do
                                let my_pos' = ahead_of my_pos my_dir
                                    new_cell = what_at m my_pos'
                                if c_type new_cell /= Rocks &&
                                   isNothing (c_ant new_cell)
                                 then do let ant' = ant { a_pos = my_pos',
                                                          a_rest = 14 }
                                             my_cell' = my_cell { c_ant = Nothing }
                                             new_cell' = new_cell { c_ant = Just aid }
                                             a' = addToFM a aid ant'
                                             m' = update_at m my_pos my_cell'
                                             m'' = update_at m' my_pos' new_cell'
                                         put $! st { st_ants = Ants a',
                                                     st_map = Map m'' }
                                         extra_cs <- mapM do_kill (all_around my_pos')
                                         set_state aid l1
                                         return ([VNoAnt my_pos,
                                                  VAnt (a_food ant)
                                                       my_col
                                                       my_pos'
                                                       (a_dir ant)
                                                       14
                                                       aid] ++ concat extra_cs)
                                 else do set_state aid l2
                                         return []
                         Flip n l1 l2 -> {-# SCC "X1" #-}
                             do
                                let (r:rs) = st_randoms st
                                    l = if r `mod` n == 0 then l1 else l2
                                put $! st { st_randoms = rs }
                                set_state aid l
                                return []

do_kill :: Pos -> M [VCommand]
do_kill p = do m_c <- get_colour p
               case m_c of
                   Nothing -> return []
                   Just c ->
                       do xs <- mapM get_colour (all_around p)
                          if length (filter (== Just (other_colour c)) xs) >= 5
                           then do st <- get
                                   let Map m = st_map st
                                       cell = what_at m p
                                       Just aid = c_ant cell
                                       food' = c_food cell + 3
                                       cell' = cell { c_ant = Nothing,
                                                      c_food = food' }
                                       m' = update_at m p cell'
                                       Ants a = st_ants st
                                       a' = delFromFM a aid
                                       redscore = if c_type cell == Anthill Red then st_redscore st + 3 else st_redscore st
                                       blackscore = if c_type cell == Anthill Black then st_blackscore st + 3 else st_blackscore st
                                   put $ st { st_map = Map m',
                                              st_ants = Ants a',
                                              st_redscore = redscore,
                                              st_blackscore = blackscore
                                            }
                                   return [VNoAnt p, VFood p food',
                                           VScore redscore blackscore]
                           else return []

get_colour :: Pos -> M (Maybe Colour)
get_colour p = do st <- get
                  let Map m = st_map st
                      Ants a = st_ants st
                  case c_ant (what_at m p) of
                      Nothing -> return Nothing
                      Just aid -> case lookupFM a aid of
                                      Just ant -> return (Just (a_col ant))
                                      Nothing -> error "Can't happen"

all_around :: Pos -> [Pos]
all_around (Pos x y)
 | even y = [Pos x y, Pos (x+1) y, Pos x (y+1), Pos (x-1) (y+1), Pos (x-1) y, Pos (x-1) (y-1), Pos x (y-1)]
 | otherwise = [Pos x y, Pos (x+1) y, Pos (x+1) (y+1), Pos x (y+1), Pos (x-1) y, Pos x (y-1), Pos (x+1) (y-1)]

set_state :: AntID -> CommandIndex -> M ()
set_state aid ci = do st <- get
                      let Ants a = st_ants st
                      case lookupFM a aid of
                          Just ant ->
                              do let ant' = ant { a_state = ci }
                                     a' = addToFM a aid ant'
                                 put $! st { st_ants = Ants a' }
                          Nothing -> return ()

cell_matches :: Pos -> Cond -> Colour -> M Bool
cell_matches p Friend c
 = do st <- get
      let Map m = st_map st
          Ants a = st_ants st
      case what_at m p of
          Cell _ (Just aid) _ _ _ ->
              case lookupFM a aid of
                  Just ant -> return $ a_col ant == c
                  Nothing -> return False
          _ -> return False
cell_matches p Foe c = cell_matches p Friend (other_colour c)
cell_matches p FriendWithFood c
 = do st <- get
      let Map m = st_map st
          Ants a = st_ants st
      case what_at m p of
          Cell _ (Just aid) _ _ _ ->
              case lookupFM a aid of
                  Just ant -> return $ a_col ant == c && a_food ant
                  Nothing -> return False
          _ -> return False
cell_matches p FoeWithFood c = cell_matches p FriendWithFood (other_colour c)
cell_matches p Food _
 = do st <- get
      let Map m = st_map st
      case what_at m p of
          Cell _ _ food _ _ -> return $ food > 0
cell_matches p Rock _
 = do st <- get
      let Map m = st_map st
      case what_at m p of
          Cell Rocks _ _ _ _ -> return True
          Cell _ _ _ _ _ -> return False
cell_matches p (Marker i) c
 = do st <- get
      let Map m = st_map st
      case (what_at m p, c) of
          (Cell _ _ _ ms _, Red) -> return $ check_marker ms i
          (Cell _ _ _ _ ms, Black) -> return $ check_marker ms i
cell_matches p FoeMarker c
 = do st <- get
      let Map m = st_map st
      case (what_at m p, c) of
          (Cell _ _ _ _ ms, Red) -> return $ check_markers ms
          (Cell _ _ _ ms _, Black) -> return $ check_markers ms
cell_matches p Home c
 = do st <- get
      let Map m = st_map st
      case what_at m p of
          Cell (Anthill c') _ _ _ _ -> return (c == c')
          Cell _ _ _ _ _ -> return False
cell_matches p FoeHome c = cell_matches p Home (other_colour c)

check_markers :: Marks -> Bool
check_markers ms = any (check_marker ms) [0..5]

check_marker :: Marks -> MarkID -> Bool
check_marker ms 0 = mark0 ms
check_marker ms 1 = mark1 ms
check_marker ms 2 = mark2 ms
check_marker ms 3 = mark3 ms
check_marker ms 4 = mark4 ms
check_marker ms 5 = mark5 ms
check_marker _  _ = error "check_marker: Can't happen"

set_marker :: Marks -> MarkID -> Bool -> Marks
set_marker ms 0 b = ms { mark0 = b }
set_marker ms 1 b = ms { mark1 = b }
set_marker ms 2 b = ms { mark2 = b }
set_marker ms 3 b = ms { mark3 = b }
set_marker ms 4 b = ms { mark4 = b }
set_marker ms 5 b = ms { mark5 = b }
set_marker _  _ _ = error "set_marker: Can't happen"

pos_of :: Pos -> Direction -> SenseDir -> Pos
pos_of p _ Here = p
pos_of p d Ahead = ahead_of p d
pos_of p d LeftAhead = pos_of p (left_of d) Ahead
pos_of p d RightAhead = pos_of p (right_of d) Ahead

left_of, right_of :: Direction -> Direction
left_of d = (d + 5) `mod` 6
right_of d = (d + 1) `mod` 6

ahead_of :: Pos -> Direction -> Pos
ahead_of (Pos x y) 0 = Pos (x+1) y
ahead_of (Pos x y) 1 = if even y then (Pos x (y+1)) else (Pos (x+1) (y+1))
ahead_of (Pos x y) 2 = if even y then (Pos (x-1) (y+1)) else (Pos x (y+1))
ahead_of (Pos x y) 3 = Pos (x-1) y
ahead_of (Pos x y) 4 = if even y then (Pos (x-1) (y-1)) else (Pos x     (y-1))
ahead_of (Pos x y) 5 = if even y then (Pos x     (y-1)) else (Pos (x+1) (y-1))
ahead_of _ _ = error "ahead_of: Can't happen"

other_colour :: Colour -> Colour
other_colour Red = Black
other_colour Black = Red

dump_map :: Map -> Ants -> [VCommand]
dump_map (Map m) (Ants a)
                  = concatMap map_cell (fmToList m) ++ map map_ant (fmToList a)
    where map_cell (p, Cell t _ f rms bms)
                  = VNoAnt p:VFood p f:do_marks p Red rms ++ do_marks p Black bms ++ cell_type
            where cell_type = case t of
                                  Rocks -> [VRock p]
                                  Anthill c -> [VAnthill c p]
                                  _ -> []
          map_ant (aid, Ant p c _ re dir food) = VAnt food c p dir re aid
          do_marks p c ms = [ VMark c p s | (b, s) <- zip (map ($ ms) [mark0, mark1, mark2, mark3, mark4, mark5]) [0..], b ]

