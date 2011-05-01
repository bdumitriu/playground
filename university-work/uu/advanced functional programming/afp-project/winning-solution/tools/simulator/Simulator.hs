
module Main (main) where

import System (getArgs)

import Parser (parse)
import Compress
import Types
import Language
import Run
import IO

data Wanted = WantCommands | WantScores | WantDump | WantProfile deriving Read

main :: IO ()
main = do args <- getArgs
          case args of
              "-compress":n1:n2:n3:rest -> main' (Just (read n1,read n2,read n3)) rest
              _ -> main' Nothing args

main' :: Maybe (Int,Int,Int) -> [String] -> IO ()
main' rang [mapfile, programfile1, programfile2, seedstring, want_commands, iters, start_at]
 = do programfile1_contents <- readFile programfile1
      programfile2_contents <- readFile programfile2
      mapfile_contents <- readFile mapfile
      let cs1 = Commands $ inputCommandsArray programfile1_contents
          cs2 = Commands $ inputCommandsArray programfile2_contents
          (m, a@(Ants a')) = parse mapfile_contents
      let (commands, s) = run (read start_at) (read iters) (read seedstring) (sizeFM a') m a cs1 cs2
          v_score (VScore _ _) = True
          v_score _ = False
          v_profile (VProfile _ _ _ _ _ _) = True
          v_profile _ = False
      let commands' = case rang of
                        Nothing -> commands
                        Just (from,to,step) -> compress [from,(from+step)..to] commands
      case read want_commands of
          WantCommands -> do write_map m
                             mapM_ print commands'
          WantScores -> case filter v_score commands' of
                            [] -> do putStrLn "Red:"
                                     print (0 :: Int)
                                     putStrLn "Black:"
                                     print (0 :: Int)
                            xs -> case last xs of
                                      VScore red black ->
                                          do putStrLn "Red:"
                                             print red
                                             putStrLn "Black:"
                                             print black
                                      _ -> error "Simulator: Can't happen"
          WantDump -> putStrLn s
	  WantProfile -> mapM_ print (filter v_profile commands')
      -- let Map m' = st_map st
      -- let s1 = sum [ n | Cell (Anthill Red) _ n _ _ <- eltsFM m' ]
      --     s2 = sum [ n | Cell (Anthill Black) _ n _ _ <- eltsFM m' ]
      -- putStrLn $ "Red's score: " ++ show s1
      -- putStrLn $ "Black's score: " ++ show s2
main' _ _ = hPutStrLn stderr "Usage: ./simulator [-compress from to step] ../maps/sample0.world ../samples/sample.ant ../samples/sample.ant 12345 WantCommands 100000 0"

write_map :: Map -> IO ()
write_map (Map m) = mapM_ (mapM_ print . map_cell) (fmToList m)
    where map_cell (p, Cell t m_a f _ _)
             = case t of
                   Rocks -> [VRock p]
                   Anthill c -> case m_a of
                                    Nothing -> error "write_map: Can't happen"
                                    Just aid -> [VAnthill c p,
                                                 VAnt False c p 0 0 aid]
                   Normal -> [VFood p f]

