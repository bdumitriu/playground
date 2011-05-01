
module Dump (dump) where

import Types

dump :: Map -> Ants -> Int -> String
dump (Map m) a i = top ++ rest ++ "\n"
    where top = "After round " ++ show i ++ "...\n"
          rest = unlines $ map (show_cell a) $ fmToList m

show_cell :: Ants -> (Pos, Cell) -> String
show_cell a (Pos x y, Cell t m_ant food redms blackms)
 = "cell (" ++ show x ++ ", " ++ show y ++ "): " ++ show_food food ++ show_t t ++ show_marks "red marks: " redms ++ show_marks "black marks: " blackms ++ show_m_ant a m_ant

show_food :: Amount -> String
show_food 0 = ""
show_food n = show n ++ " food; "

show_t :: Type -> String
show_t Rocks = "rock"
show_t (Anthill Red) = "red hill; "
show_t (Anthill Black) = "black hill; "
show_t Normal = ""

show_marks :: String -> Marks -> String
show_marks s (Marks m0 m1 m2 m3 m4 m5)
 = case concat (zipWith show_mark [m0, m1, m2, m3, m4, m5] [0..5 :: Int]) of
       "" -> ""
       xs -> s ++ xs ++ "; "
    where show_mark True i = show i
          show_mark False _ = ""

show_m_ant :: Ants -> Maybe AntID -> String
show_m_ant _ Nothing = ""
show_m_ant (Ants a) (Just aid) = case lookupFM a aid of
                                     Nothing -> ""
                                     Just ant -> show_ant aid ant

show_ant :: AntID -> Ant -> String
show_ant aid (Ant _ c st rest dir food) = show_c c ++ " ant of id " ++ show aid ++ ", dir " ++ show dir ++ ", food " ++ show_hasfood food ++ ", state " ++ show st ++ ", resting " ++ show rest

show_c :: Colour -> String
show_c Red = "red"
show_c Black = "black"

show_hasfood :: HasFood -> String
show_hasfood False = "0"
show_hasfood True = "1"

