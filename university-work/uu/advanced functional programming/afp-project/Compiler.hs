-- | The main module of the compiler for
-- High Level ant instructions.
module Compiler where

import Ant
import Antcode
import Cell
import HLAntCode
import HLAntCombinators hiding (goto)
import ExampleHLAntCode
import Control.Monad.State
import Data.FiniteMap
import Prelude hiding(Left, Right)

-- | The type synonym for the environment to store labels
--   Note that FiniteMap is deprecated, but it
--   ensures compatibility with GHC 6.2.2
type Env = FiniteMap String Int

-- | The state used in the compilation process, which is based on a state
--   monad.
data CompilerState = CompilerState { ic     :: Int              -- ^ the instruction counter
                                   , inDo   :: Maybe Int        -- ^ Nothing if not inside a DoUntil,
                                                                --   Just ic, where ic is the instruction count
                                                                --   which should be used by a break instruction
                                                                --   inside the DoUntil
                                   , jumpTo :: Maybe (Int, Int) -- ^ Nothing if instructions are to continue
                                                                --   sequentially,
                                                                --   Just (x,y) if instruction at instruction
                                                                --   count x should jump to y after running
                                   , env :: Env                 -- ^ Environment for the gotos.
                                   , compileGotoF :: Bool       -- ^ Flag to track if we want to compile goto's 
                                                                --   or not
                                   }

-- | Initial, empty 'CompilerState'
emptyCompilerState = CompilerState 0 Nothing Nothing emptyFM False
                                   
-- | A type synonym for the state monad representing the compiler.
type Compiler a = State CompilerState a

-- | Outputs low level code from the given 'HLI'
outputCompile :: HLI -> IO ()
outputCompile i = mapM_ (\(x, y) -> putStrLn (show x ++ " ; State " ++ show y)) (zip (compile i) [0 .. ])

-- | Returns the 'compileGotoF' value of the 'CompilerState'.
getCompileGoto :: Compiler Bool
getCompileGoto = get >>= return . compileGotoF

-- | Returns the 'env' value of the 'CompilerState'.
getEnv :: Compiler Env
getEnv = get >>= return . env

-- | Sets the 'env' value of the 'CompilerState'.
putEnv :: Env -> Compiler ()
putEnv e = do x <- get
              put (x { env = e })

-- | Returns the 'ic' value of the 'CompilerState'.
getIC :: Compiler Int
getIC = get >>= return . ic

-- | Sets the 'ic' value of the 'CompilerState'.
putIC :: Int -> Compiler ()
putIC count = do x <- get
                 put (x { ic = count })

-- | Returns the 'inDo' value of the 'CompilerState'.
getInDo :: Compiler (Maybe Int)
getInDo = get >>= return . inDo

-- | Sets the 'inDo' value of the 'CompilerState'.
putInDo ::  (Maybe Int) -> Compiler ()
putInDo indo = do x <- get
                  put (x { inDo = indo })

-- | Returns the 'jumpTo' value of the 'CompilerState'.
getJumpTo :: Compiler (Maybe (Int, Int))
getJumpTo = get >>= return . jumpTo

-- | Sets the 'jumpTo' value of the 'CompilerState'.
putJumpTo ::  (Maybe (Int, Int)) -> Compiler ()
putJumpTo jt = do x <- get
                  put (x { jumpTo = jt })

-- | Compiles an 'HLI' to a set of low level instructions.
compile :: HLI -> [Instruction]
compile i = evalState stateMonad (CompilerState 0 Nothing Nothing preProc True)
  where 
        preProc = env (execState stateMonad emptyCompilerState)
        stateMonad = compilationScheme i

compilationScheme :: HLI -> Compiler [Instruction]
compilationScheme CDrop                 = compileDrop
compilationScheme (CTurn x)             = compileTurn x
compilationScheme (CMark x)             = compileMark x
compilationScheme (CUnmark x)           = compileUnmark x
compilationScheme CNop                  = compileNop
compilationScheme CBreak                = compileBreak
compilationScheme (CSeq i1 i2)          = compileSeq i1 i2
compilationScheme (CIfThenElse c ti ei) = compileIf c ti ei
compilationScheme (CDoUntil c li)       = compileDo c li
compilationScheme (CWhile c li)         = compileWhile c li
compilationScheme (Label x)             = compileLabel x
compilationScheme (Goto x)              = compileGoto x

-- | Compiles the 'Label' instruction.
-- NOTE: Don't use the same label twice.
compileLabel :: String -> Compiler [Instruction]
compileLabel x = do ic <- getIC
                    e <- getEnv
                    putEnv (addToFM e x ic)
                    return []

-- | Compiles the 'Goto' instruction. Note that if we are 
-- just preprocessing the labels, this doesn't really compile,
-- just increments the IC.
compileGoto :: String -> Compiler [Instruction]
compileGoto x = do b <- getCompileGoto
                   if b
                    then
                     (do e <- getEnv
                         let res' = lookupFM e x
                             res = maybe (error ("Label '" ++ x ++ "' invalid")) id res'
                         compileGotoIn res)
                    else
                     (nextIC >> return [])

-- | Compiles the 'CDrop' instruction.
compileDrop :: Compiler [Instruction]
compileDrop = do ic <- nextIC
                 ic2 <- checkJumpTo ic
                 return [Drop ic2]

-- | Compiles the 'CTurn' instruction.
compileTurn :: LeftOrRight -> Compiler [Instruction]
compileTurn dir = do ic <- nextIC
                     ic2 <- checkJumpTo ic
                     return [Turn dir ic2]

-- | Compiles the 'CMark' instruction.
compileMark :: Marker -> Compiler [Instruction]
compileMark m = do ic <- nextIC
                   ic2 <- checkJumpTo ic
                   return [Mark m ic2]

-- | Compiles the 'CUnmark' instruction.
compileUnmark :: Marker -> Compiler [Instruction]
compileUnmark m = do ic <- nextIC
                     ic2 <- checkJumpTo ic
                     return [Unmark m ic2]

-- | Compiles the 'CNop' instruction.
compileNop :: Compiler [Instruction]
compileNop = return []

-- | Compiles the 'CBreak' instruction.
compileBreak :: Compiler [Instruction]
compileBreak = do indo <- getInDo
                  ic <- nextIC
                  case indo of
                    Nothing -> do ic2 <- checkJumpTo ic
                                  return [goto ic2]
                    Just x  -> do ic2 <- checkJumpTo x
                                  return [goto x]

-- | Compiles the internal goto instruction.
compileGotoIn :: Int -> Compiler [Instruction]
compileGotoIn x = do ic <- nextIC
                     return [goto x]

-- | Compiles the 'CSeq' instruction.
compileSeq :: HLI -> HLI -> Compiler [Instruction]
compileSeq i1 i2 = do i1compiled <- compilationScheme i1
                      i2compiled <- compilationScheme i2
                      return (i1compiled ++ i2compiled)

-- | Compiles the 'CIfThenElse' instruction.
--   The code is optimized for 'CNop' instruction.
compileIf :: Cond -> HLI -> HLI -> Compiler [Instruction]
compileIf ci ti ei = do let ciSize = condSize ci
                        let tiSize = codeSize ti
                        let eiSize = codeSize ei
                        ciIC <- getIC
                        let tiIC = ciIC + ciSize
                        let eiIC = tiIC + tiSize
                        let endIC = eiIC + eiSize
                        -- for nested ifs, we need to take extra care in the way we
                        -- set the jumpTo value for the inner then
                        -- as well as for 'CNop' instruction
                        endIC2 <- checkJumpTo endIC
                        let tiIC2 = if tiSize == 0 then endIC2 else tiIC
                        let eiIC2 = if eiSize == 0 then endIC2 else eiIC                        
                        ciCompiled <- compileCond ci tiIC2 eiIC2
                        oldJumpTo <- getJumpTo
                        putJumpTo (Just (eiIC, endIC2))
                        tiCompiled <- compilationScheme ti
                        putJumpTo oldJumpTo
                        eiCompiled <- compilationScheme ei
                        return (ciCompiled ++ tiCompiled ++ eiCompiled)

-- | Compiles the 'CDoUntil' instruction.
compileDo :: Cond -> HLI -> Compiler [Instruction]
compileDo ci li = do let ciSize = condSize ci
                     let liSize = codeSize li
                     liIC <- getIC
                     let ciIC = liIC + liSize
                     let endIC = ciIC + ciSize
                     endIC2 <- checkJumpTo endIC
                     oldInDo <- getInDo
                     putInDo (Just endIC2)
                     liCompiled <- compilationScheme li
                     ciCompiled <- compileCond ci endIC2 liIC
                     putInDo oldInDo
                     return (liCompiled ++ ciCompiled)

-- | Compiles the 'CWhile' instruction.
compileWhile :: Cond -> HLI -> Compiler [Instruction]
compileWhile ci li = do let ciSize = condSize ci
                        let liSize = codeSize li
                        ciIC <- getIC
                        let liIC = ciIC + ciSize
                        let endIC = liIC + liSize + 1
                        endIC2 <- checkJumpTo endIC
                        oldInDo <- getInDo
                        putInDo (Just endIC2)
                        ciCompiled <- compileCond ci liIC endIC2
                        liCompiled <- compilationScheme li
                        lpCompiled <- compileGotoIn ciIC
                        putInDo oldInDo
                        return (ciCompiled ++ liCompiled ++ lpCompiled)

-- | Compiles the 'Cond'ition of a 'CIfThenElse' or 'CDoUntil' instruction.
compileCond :: Cond -> Int -> Int -> Compiler [Instruction]
compileCond (CSense dir cnd) icTrue icFalse = do
                ic <- nextIC
                return $ [Sense dir icTrue icFalse cnd]
                                                     
compileCond (CMove         ) icTrue icFalse = do
                ic <- nextIC
                return $ [Move icTrue icFalse]
                                                     
compileCond (CPickUp       ) icTrue icFalse = do
                ic <- nextIC
                return $ [PickUp icTrue icFalse]
                                                     
compileCond (CFlip i       ) icTrue icFalse = do
                ic <- nextIC
                return $ [Flip i icTrue icFalse]
                                                     
compileCond (CNot cnd      ) icTrue icFalse =
                compileCond cnd icFalse icTrue

compileCond (CAnd cnd1 cnd2) icTrue icFalse = do
                cnd1IC <- getIC
                let cnd1Size = condSize cnd1
                let cnd2IC = cnd1IC + cnd1Size
                cnd1Compiled <- compileCond cnd1 cnd2IC icFalse
                cnd2Compiled <- compileCond cnd2 icTrue icFalse
                return (cnd1Compiled ++ cnd2Compiled)

compileCond (COr cnd1 cnd2) icTrue icFalse = do
                cnd1IC <- getIC
                let cnd1Size = condSize cnd1
                let cnd2IC = cnd1IC + cnd1Size
                cnd1Compiled <- compileCond cnd1 icTrue cnd2IC
                cnd2Compiled <- compileCond cnd2 icTrue icFalse
                return (cnd1Compiled ++ cnd2Compiled)
                
compileCond (CTrue        ) icTrue icFalse = do
                ic <- nextIC
                return $ [goto icTrue] 

compileCond (CFalse       ) icTrue icFalse = do
                ic <- nextIC
                return $ [goto icFalse] 

-- | Increments the instruction count in the 'CompilerState'.
nextIC :: Compiler Int
nextIC = do ic <- getIC
            let ic2 = ic + 1
            putIC ic2
            return ic2

-- | Checks the instruction count which it receives as parameter, and if it
--   matches the instruction count mentioned in the 'jumpTo' of the
--   'CompilerState', it returns the value to jump to, otherwise it just
--   returns the next instruction count (jump to next = normal execution).
checkJumpTo :: Int -> Compiler Int
checkJumpTo ic = do jt <- getJumpTo
                    case jt of
                      Nothing     -> return ic
                      Just (x, y) -> if ic == x then return y else return ic

-- | Returns the code size of a 'HLI'.
codeSize :: HLI -> Int
codeSize (CSeq i1 i2)          = codeSize i1 + codeSize i2
codeSize (CIfThenElse c ti ei) = condSize c + codeSize ti + codeSize ei
codeSize (CDoUntil c li)       = condSize c + codeSize li
codeSize (CWhile c li)         = condSize c + codeSize li + 1
codeSize (CNop)                = 0
codeSize (Label _)             = 0
codeSize _                     = 1

-- | Returns the code size of a 'Cond'.
condSize :: Cond -> Int
condSize (CNot c)     = condSize c
condSize (CAnd c1 c2) = condSize c1 + condSize c2
condSize (COr c1 c2)  = condSize c1 + condSize c2
condSize _            = 1

-- | Goto instruction which is implemented with Sense.
goto :: Int -> Instruction
goto x = Flip 1 x x
