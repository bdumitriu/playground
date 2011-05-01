{-# OPTIONS -fglasgow-exts #-}

module StateMonadPlus(StateMonadPlusT, StateMonadPlus, diagnostics, 
       runStateMonadPlus, annotate,
       saveState, loadState, module Control.Monad.State) where

import Stack
import Control.Monad.Error
import Control.Monad.State hiding(runStateT)

type DiagnosticsInfo = [(String, Int)]
type ErrorInfo       = String
data ExtraInfo s     = ExtraInfo { diag :: DiagnosticsInfo, stack :: Stack s }

data StateMonadPlusT s m a = SMP { runStateT :: (ExtraInfo s, s) -> m (a, (ExtraInfo s, s)), label :: String }


-- Creates a new StateMonadPlusT, with the given transition function and label
newSMPT :: (Monad m) => ((ExtraInfo s, s) -> m (a, (ExtraInfo s, s))) -> String -> StateMonadPlusT s m a
newSMPT f s = SMP { runStateT = f, label = s }

-- Creates a new transition function which keeps the state and updates the ExtraInfo with the function given
newRunStateT :: (Monad m) => a -> (DiagnosticsInfo -> DiagnosticsInfo) -> (ExtraInfo s, s) -> m (a, (ExtraInfo s, s))
newRunStateT a update (e,s) = return (a, (e { diag = update (diag e) }, s))

instance (Monad m) => Monad (StateMonadPlusT s m) where
  (SMP x l) >>= f = newSMPT g l
    where g (e,s) = do (v, (e', s')) <- x (e, s)
		       (SMP x' l') <- return $ f v
		       x' (e' { diag = update "(>>=)" (diag e') }, s')
      
  return a = newSMPT g ""
    where g = newRunStateT a (update "return")

  fail s = newSMPT (const (fail s)) ""


instance (Monad m) => MonadState s (StateMonadPlusT s m) where
  get = newSMPT g "" 
    where g (e,s) = newRunStateT s (update "get") (e,s)
  
  put s = newSMPT g ""
    where g (e,_) = return ((), (e { diag = update "put" (diag e) }, s))

instance MonadTrans (StateMonadPlusT s) where
  lift c = newSMPT g ""
    where g (e,s) = c >>= (\x -> newRunStateT x (update "lift") (e,s))

-- The StoreState class
class MonadState s m => StoreState s m | m -> s where
  saveState :: m ()
  loadState :: m ()

instance (Monad m) => StoreState s (StateMonadPlusT s m) where
  saveState = newSMPT g ""
    where g (e,s) = return ((),(e { stack = push s (stack e) },s))

  loadState = newSMPT g ""
    where g (e,_) = case pop (stack e) of
		      Nothing -> fail "trying to load state from empty stack"
		      Just (v,st) -> return ((),(e { stack = st },v))

--
-- Returns a state monad transformer which has a diagnostics string as its
-- value.
--
diagnosticsT :: Monad m => StateMonadPlusT s m String
diagnosticsT = newSMPT g ""
  where g (e,s) = let e' = e { diag = update "diagnostics" (diag e) }
                  in return (showDiagnosticsInfo (diag e'), (e', s))

--
-- Annotates the state monad with the specified label.
--
annotateT :: (Monad m) => String -> StateMonadPlusT s m a -> StateMonadPlusT s m a
annotateT s smp = smp { label = s }

--
-- Runs the state monad, given a computation and an initial state.
-- Returns either an error message or a (value, final-state) pair.
--
runStateMonadPlusT :: (Monad m) => StateMonadPlusT s m a -> s -> m (a, s)
runStateMonadPlusT (SMP rs _) s = do (a, (e, s')) <- rs (initExtraInfo, s)
                                     return (a, s')

--
-- A StateMonadPlus that allows errors.
--
type StateMonadPlus s a = StateMonadPlusT s (Either String) a

--
-- Returns a state monad which has a diagnostics string as its value.
--
diagnostics :: StateMonadPlus s String
diagnostics = diagnosticsT

--
-- Annotates the state monad with the specified label.
--
annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate = annotateT

--
-- Runs the state monad, given a computation and an initial state.
-- Returns either an error message or a (value, final-state) pair.
--
runStateMonadPlus :: StateMonadPlus s a -> s -> Either String (a, s)
runStateMonadPlus = runStateMonadPlusT


{- Functions for the DiagnosticsInfo type -}

initExtraInfo :: ExtraInfo a
initExtraInfo = ExtraInfo initDiagnosticsInfo emptyStack

initDiagnosticsInfo :: DiagnosticsInfo
initDiagnosticsInfo = []

--
-- Updates a DiagnosticsInfo list by adding 1 to the number of calls made to
-- <func> if an entry (<func>, nrCalls) exists or by inserting an entry
-- (<func>, 1) otherwise.
--
update :: String -> DiagnosticsInfo -> DiagnosticsInfo
update func []         = [(func, 1)]
update func ((f, c):t) | f == func = (func, succ c):t
                       | otherwise = (f, c):(update func t)

--
-- Returns a user-friendly representation of the diagnostics information.
--
showDiagnosticsInfo :: DiagnosticsInfo -> String
showDiagnosticsInfo info = '[' : (showPairs info ++ "]")
  where showPairs = concat . separateWith ", " . map showPair
        showPair (func, nrCalls) = func ++ "=" ++ show nrCalls

--
-- Adds separators to a list.
--
separateWith :: a -> [a] -> [a]
separateWith _ []       = []
separateWith _ [x]      = [x]
separateWith sep (x:xs) = x : sep : (separateWith sep xs)
