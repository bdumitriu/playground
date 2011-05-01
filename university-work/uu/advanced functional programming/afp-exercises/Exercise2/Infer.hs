{-# OPTIONS -fglasgow-exts #-}
-- compile with:
--    ghc --make -fglasgow-exts -o Infer.exe Infer

module Main where

import Control.Monad            (unless)
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Trans      (lift)
import Control.Arrow             
import Data.List                (union, delete, (\\))
import System.IO                (hFlush, stdout)
import UU_Scanner
import UU_Parsing_Core
import UU_Parsing_Derived 

------------------------------------------------------------
-- 1. Types and type schemes

infixr 5 :->:

data Scheme = Forall Int Scheme
            | Simple Type
   
data Type = TVar Int        -- type variable
          | TCon String     -- type constant
          | Type :->: Type  -- function type

type StringError    = Either String
type ExprErrorT m   = ErrorT (Expr -> Expr -> String) m
type StringErrorT m = ErrorT String m
type StateInfo      = ((Int, Substitution), Gamma)
type TypeInfState a = StateT StateInfo (StringErrorT (Writer ShowS)) a

instance Control.Monad.Error.Error (Expr -> Expr -> String)
  

-- Custom set's and get's for the (our) StateT monad
getSubst :: TypeInfState Substitution
getSubst = StateT $ \e@((i, s), g) -> return (s,e)
           
getI :: TypeInfState Int
getI = StateT $ \e@((i, s), g) -> return (i,e)
              
getGamma :: TypeInfState Gamma
getGamma = StateT $ \e@((i, s), g) -> return (g,e)
              
putSubst :: Substitution -> TypeInfState ()
putSubst s = StateT $ \((i, _), g) -> return ((),((i, s), g))

putI :: Int -> TypeInfState ()
putI i = StateT $ \((_, s), g) -> return ((),((i, s), g))

putGamma :: Gamma -> TypeInfState ()
putGamma g = StateT $ \((i, s), _) -> return ((),((i, s), g))

modifySubst :: (Substitution -> Substitution) -> TypeInfState ()
modifySubst f = modify ((id *** f) *** id)

modifyI :: (Int -> Int) -> TypeInfState ()
modifyI f = modify ((f *** id) *** id) 

modifyGamma :: (Gamma -> Gamma) -> TypeInfState ()
modifyGamma f = modify ((id *** id) *** f) 

bool, int :: Type
bool = TCon "Bool"
int  = TCon "Int"

instance Show Scheme where
   show = let f _ (Simple t)   = show t
              f c (Forall i s) = f (succ c) (singletonSubst i (TCon [c]) |-> s) 
          in f 'a'

instance Show Type where
   show (TVar i)     = "v" ++ show i
   show (TCon s)     = s
   show (t1 :->: t2) = 
      let isFunction (_ :->: _) = True
          isFunction _          = False
      in parIf (isFunction t1) (show t1) ++ " -> " ++ show t2

parIf :: Bool -> String -> String
parIf b s = if b then "("++s++")" else s

------------------------------------------------------------
-- 2. Substitutions

infix 3 |->

type Substitution = Int -> Type

class Substitutable a where
   vars  :: a -> [Int]
   (|->) :: Substitution -> a -> a
   
instance Substitutable Type where
   vars (TVar i)     = [i]
   vars (t1 :->: t2) = vars t1 `union` vars t2
   vars _            = []
   
   sub |-> (TVar i)     = sub i
   sub |-> (t1 :->: t2) = (sub |-> t1) :->: (sub |-> t2)
   _   |-> t            = t

instance Substitutable Scheme where
   vars (Forall i s) = delete i (vars s)
   vars (Simple t)   = vars t
   
   sub |-> Forall i t = Forall i (removeFromSubst i sub |-> t)
   sub |-> Simple t   = Simple (sub |-> t)

instance Substitutable a => Substitutable [a] where
   vars = foldr (union . vars) []
   sub |-> as = map (sub |->) as

emptySubst :: Substitution
emptySubst = TVar

singletonSubst :: Int -> Type -> Substitution
singletonSubst i t = \j -> if i==j then t else TVar j

(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2 = (s1 |->) . s2

removeFromSubst :: Int -> Substitution -> Substitution
removeFromSubst i s = \j -> if i==j then TVar j else s j

------------------------------------------------------------
-- 3. A small expression language

data Expr = Var String            -- variable
          | Int Int               -- integer constant
          | Bool Bool             -- boolean constant
          | Apply Expr Expr       -- application
          | Lambda String Expr    -- lambda abstraction
          | Cond Expr Expr Expr   -- conditional
          | Let String Expr Expr  -- let expression

instance Show Expr where
   show (Var s)      = s
   show (Int i)      = show i
   show (Bool b)     = show b
   show (Apply f a)  = parIf (prioExpr f < 2) (show f) ++ " " ++ parIf (prioExpr a < 3) (show a)
   show (Lambda x e) = "\\" ++ x ++ " -> " ++ show e
   show (Cond b t e) = "if " ++ show b ++ " then " ++ show t ++ " else " ++ show e ++ " fi"
   show (Let x b d)  = "let " ++ x ++ " = " ++ show b ++ " in " ++ show d ++ " ni"

prioExpr :: Expr -> Int
prioExpr (Lambda _ _) = 1
prioExpr (Apply _ _)  = 2
prioExpr _            = 3

------------------------------------------------------------
-- 4. (Type) environments       
         
newtype Env a = Env [(String, a)]
type Gamma = Env Scheme

emptyEnv :: Env a
emptyEnv = Env []

extendEnv :: (Show a) => String -> a -> Env a -> Writer ShowS (Env a)
extendEnv s t (Env xs) = do tell (("(\"" ++ s ++ "\", " ++ show t ++ ") in Gamma\n") ++)
                            return (Env ((s, t) : xs))

findInEnv :: String -> Env a -> Maybe a
findInEnv s (Env xs) = lookup s xs

instance Substitutable a => Substitutable (Env a) where
   vars (Env xs)  = vars (map snd xs)
   sub |-> env    = fmap (sub |->) env

instance Functor Env where
   fmap f (Env xs) = Env [ (s, f a) | (s, a) <- xs ]

------------------------------------------------------------
-- 5. Type inference utility functions

mgu :: Type -> Type -> ExprErrorT (Writer ShowS) Substitution
mgu (TVar i) t@(TVar j)
   | i == j    = return emptySubst
   | otherwise = varBind i t
mgu (TVar i) t = varBind i t
mgu t (TVar i) = varBind i t
mgu (TCon s) (TCon t)
   | s == t    = return emptySubst
   | otherwise = throwError (genHugslikeError (TCon s) (TCon t) "Type constants clash")
mgu (t1 :->: t2) (t3 :->: t4) =
   do s <- mgu t1 t3
      s1 <- mgu (s |-> t2) (s |-> t4)
      return (s1 @@ s)
mgu t1 t2 = throwError (genHugslikeError t1 t2 "Type constant and function type clash")

varBind :: Int -> Type -> ExprErrorT (Writer ShowS) Substitution
varBind i t
   | i `elem` vars t = throwError (genHugslikeError (TVar i) t "Unification would give infinite type")
   | otherwise       = do tell (("Unifying " ++ show (TVar i) ++ " and " ++ show t ++ "\n" ++ show (TVar i) ++ " := " ++ show t ++ "\n") ++)
                          return (singletonSubst i t)

generalize :: Substitutable a => a -> Type -> Scheme
generalize a t = foldr Forall (Simple t) (vars t \\ vars a)

instantiate :: Scheme -> TypeInfState Type
instantiate (Simple t)   = return t
instantiate (Forall i s) =
   do t <- instantiate s
      modifyI succ
      n <- getI
      return (singletonSubst i (TVar (pred n)) |-> t)

genHugslikeError :: Type -> Type -> String -> Expr -> Expr -> String
genHugslikeError t1 t2 s e1 e2 = 
  "ERROR - Type error in application" ++
  "\n*** Expression     : " ++ show e1 ++
  "\n*** Term           : " ++ show e2 ++
  "\n*** Type           : " ++ show t1 ++
  "\n*** Does not match : " ++ show t2 ++
  "\n*** Because        : " ++ s ++ "\n"


------------------------------------------------------------
-- 6. Type inference

infer :: Expr -> (StringError Scheme, String)
infer expr =
  ((right id) *** ($ "")) . runWriter . runErrorT 
  . evalStateT (inferScheme expr) $ ((0, emptySubst), emptyEnv)

inferScheme :: Expr -> TypeInfState Scheme
inferScheme expr = 
        do gamma <- getGamma
           t <- inferType expr
           s' <- getSubst
           return (generalize (s' |-> gamma) t)

-- Function application with 2 arguments
($$) :: (a -> b -> c) -> (a, b) -> c
($$) f (x, y) = f x y

applyExprs :: Expr -> Expr -> ExprErrorT (Writer ShowS) Substitution -> TypeInfState Substitution
applyExprs e1 e2 m = lift (mapErrorT (mapWriter (left ($$ (e1, e2)) *** id)) m)

inferType :: Expr -> TypeInfState Type
inferType expr =
  let body =
        do ((i, subst), gamma) <- get 
           case expr of
        
              Var s ->
                 case findInEnv s gamma of
                    Nothing     -> lift (throwError ("Unbound variable " ++ show s))
                    Just scheme -> instantiate scheme

              Int _  -> 
                 return int
        
              Bool _ -> 
                 return bool
        
              Apply e1 e2 ->
                 do t1 <- inferType e1
                    s1 <- getSubst
                    modifyGamma (s1 |->)
                    t2 <- inferType e2
                    s2 <- getSubst
                    newI <- getI
                    new <- return (TVar newI)
                    s3 <- applyExprs (Apply e1 e2) e2 (mgu (s2 |-> t1) (t2 :->: new))
                    modifyI succ
                    putSubst (s3 @@ s2 @@ s1)
                    return (s3 |-> new)
              
              Lambda x e -> 
                 let new = TVar i
                 in do modifyI succ
                       newGamma <- lift . lift $ extendEnv x (Simple new) gamma
                       putGamma newGamma
                       t1 <- inferType e
                       s1 <- getSubst
                       return ((s1 |-> new) :->: t1)
        
              Cond e1 e2 e3 ->
                 do t1 <- inferType e1
                    s1 <- getSubst
                    modifyGamma (s1 |->)
                    t2 <- inferType e2
                    s2 <- getSubst
                    modifyGamma (s2 @@ s1 |->)
                    t3 <- inferType e3
                    s3 <- getSubst
                    s4 <- applyExprs (Cond e1 e2 e3) e1 (mgu t1 bool)
                    s5 <- applyExprs (Cond e1 e2 e3) e2 (mgu (s4 @@ s3 |-> t2) (s4 |-> t3))
                    putSubst (s5 @@ s4 @@ s3 @@ s2 @@ s1)
                    return (s5 @@ s4 |-> t3)

              Let x e1 e2 -> 
                 do scheme <- inferScheme e1
                    s1 <- getSubst
                    newGamma <- lift . lift $ extendEnv x scheme gamma
                    putGamma newGamma
                    tp <- inferType e2
                    modifySubst (@@ s1)
                    return tp
  in do gamma <- getGamma
        putSubst emptySubst
        result <- body
        z <- tell ((show expr ++ " :: " ++ show result ++ "\n") ++)
        putGamma gamma
        return result

------------------------------------------------------------
-- 7. Main functions

main :: IO ()
main = putStrLn (unlines signature) >> evalStateT (forever mainOnce) False >> return ()

mainOnce :: StateT Bool IO Bool
mainOnce =
   do input <- liftIO $ do putStr "> "
                           hFlush stdout -- flushes the command-prompt (only needed for putStr)
                           getLine
      (case input of
          (':':rest) -> do tokens <- liftIO (cScanner rest)
                           (case parse pCommand tokens of
                              (Quit, [])       -> return True
                              (Debug b, [])    -> put b >>
                                                  liftIO (if b then putStrLn "Turning debugging on." else putStrLn "Turning debugging off.") >>
                                                  return False
                              (QueryDebug, []) -> do b <- get
                                                     liftIO (if b then putStrLn "Debugging is currently on." else putStrLn "Debugging is currently off.")
                                                     return False
                              (_, es)          -> (liftIO . putStrLn $ ("Unknown command: ':" ++ rest ++ "'"))
                                                  >> return False)
          []         -> return False
          otherwise  -> do tokens <- liftIO (hsScanner input)
                           case parse pExpr tokens of
                             (expr, []) -> let (se, debugInfo) = infer expr
                                           in do debug <- get
                                                 printData <- return (buildPrintData debug debugInfo expr se)
                                                 liftIO (putStrLn printData)
                                                 return False
                             (_   , es) -> liftIO $ do putStrLn "Syntax error:" 
                                                       putStrLn es
                                           >> return False)

forever :: Monad m => m Bool -> m Bool
forever m = do quit <- m
               if quit then return True else forever m

signature :: [String]
signature =
   [ "   _   ___ ___" 
   , "  /_\\ | __| _ \\        A Monadic Type Inferencer"
   , " / _ \\| _||  _/                (May 2005)"
   , "/_/ \\_\\_| |_|"
   ]

buildTypeData :: (Show a) => Expr -> StringError a -> String
buildTypeData expr (Left se)  = se
buildTypeData expr (Right se) = show expr ++ " :: " ++ (show se)

   
buildPrintData :: (Show a) => Bool -> String -> Expr -> StringError a -> String
buildPrintData True "" expr se        = buildTypeData expr se
buildPrintData True debugInfo expr se = debugInfo ++ "\n" ++ buildTypeData expr se
buildPrintData False _ expr se        = buildTypeData expr se

------------------------------------------------------------
-- 8. An expression scanner

keywordstxt, keywordsops :: [String]
specialchars, opchars :: [Char]
keywordstxt      = [ "if", "then", "else", "fi"
                   , "let",  "in", "ni"
                   , "True", "False"
                   , "Int", "Bool" 
                   ]
keywordsops      = [ "=", "\\", "->", "::", "@"]
specialchars     = "();,"
opchars          = "!#$%&*+/<=>?@\\^|-:."

hsScanner :: String -> IO [Token]
hsScanner = scanner False keywordstxt keywordsops specialchars opchars "(...)" . Just

------------------------------------------------------------
-- 9. An expression parser

pExpr, pApp, pFactor :: Parser Token Expr
pExpr   =  flip (foldr Lambda) 
           <$ pKey "\\"  <*> pList1 pVarid
           <* pKey "->"  <*> pExpr
       <|> pApp

pApp    = pChainl (pSucceed Apply) pFactor

pFactor =     pParens pExpr
          <|> (Int . read) <$> pInteger
          <|> Bool <$> (  True  <$ pKey "True"
                      <|> False <$ pKey "False"
                       )
          <|> Var <$> pVarid 
          <|> Cond <$ pKey "if"    <*> pExpr
                          <* pKey "then"  <*> pExpr
                          <* pKey "else"  <*> pExpr
                          <* pKey "fi"
          <|> Let <$ pKey "let" <*> pVarid <* pKey "=" <*> pExpr
                  <* pKey "in"  <*> pExpr
                  <* pKey "ni"

------------------------------------------------------------
-- 10. A command scanner

cKeywords :: [String]
cKeywords = [ "q", "debug", "on", "off" ]

cScanner :: String -> IO [Token]
cScanner = scanner False cKeywords [] "" "" "(...)" . Just

------------------------------------------------------------
-- 10. A command parser

data Command = Quit
             | Debug Bool
             | QueryDebug

pCommand :: Parser Token Command
pCommand =     Quit <$ pKey "q"
           <|> Debug True <$ pKey "debug" <* pKey "on"
           <|> Debug False <$ pKey "debug" <* pKey "off"
           <|> QueryDebug <$ pKey "debug"
