
module Parser (parse) where

import Types

no_marks :: Marks
no_marks = Marks False False False False False False

parse :: String -> (Map, Ants)
parse xs = parse_lines $ lines $ xs

parse_lines :: [String] -> (Map, Ants)
parse_lines (ws:hs:xs) = (Map m', Ants ants_array)
    where w = read ws
          h = read hs
          clean = filter ('\n' /=) . filter (' ' /=) . unlines
          cells = map make_cell (clean xs)
          m = listToFM
            $ zip [ Pos x y | y <- [0..h-1], x <- [0..w-1] ] cells
          ant_infos = [ Ant p c 0 0 0 False
                      | (p, Cell (Anthill c) _ _ _ _) <- fmToList m ]
          ants_array = listToFM $ zip [0..] ant_infos
          m' = addListToFM m [ (a_pos a, Cell (Anthill (a_col a)) (Just aid)
                                              0 no_marks no_marks)
                             | (aid, a) <- fmToList ants_array ]
parse_lines _ = error "parse_lines: Can't happen"

make_cell :: Char -> Cell
make_cell '#' = Cell Rocks Nothing 0 no_marks no_marks
make_cell '+' = Cell (Anthill Red) Nothing 0 no_marks no_marks
make_cell '-' = Cell (Anthill Black) Nothing 0 no_marks no_marks
make_cell '.' = Cell Normal Nothing 0 no_marks no_marks
make_cell c = Cell Normal Nothing (read [c]) no_marks no_marks

