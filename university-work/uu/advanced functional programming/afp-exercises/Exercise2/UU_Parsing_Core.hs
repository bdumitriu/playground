{- Fast, Error Correcting Parser Combinators
 - Copyright:  S. Doaitse Swierstra
               Department of Computer Science
               Utrecht University
               P.O. Box 80.089
               3508 TB UTRECHT  
               the Netherlands
               swierstra@cs.uu.nl
 - version 2000, Sep 6 : a) fixed small error in cmpSR 
                       : b) improved handling at eof with greedy interference
                       : c) different error messages handling
               , Sep 7 : swapping, but not fully adeqaute
               ,     9 : Dynamic is more agressive in binding
                         pSucceed is now dynamic
                         use pSSucceed for static
               , Sep 12a: changes lookahead at eof
               , Sep 12b: reformulated mkparser
               , Sep 13a: many lengths removed
 - version 2001, Feb 7 : completely different basic parsing engine
 - version 2001, Feb 25: length reintroduced
                         `best` slightly reformulated, accumulated costs
                                                     , limited lookahead
                         no eof token anymore
                         class symbol is simplified
 - version 2001, Feb 28: corr0, corr1, faster locating
                       : different way of adding error steps, 0-5 should work faster now
                       : introduction of Descr
 - version 2001, Mar  1: Dyn-amics put in again (was quite tricky)
                       : <<|> and <||> re-implemented
 - version 2001, Mar 2 : Trace replaced by IOExts
                       : error messages also report symbol after correction
                       : ~'s added to ensure lazy matching
 - version 2001, mar 3 : changed architecture of pFind
                       : refined <*> for better incorporation of empties
                       : only calls to usererror or systemeror
                       : mkParser is a bit faster
 - version 2001, mar 7 : made error messages more generic
 - version 2001, mar 8 : type signatures added
                       : Binary Trees take out to separate module
 - version 2001, mar 9 : small update of mkparser
         
-}

module UU_Parsing_Core 
 ( pSucceed, pFail, pLRange, pSym, (<..>), (<|>), (<<|>), (<*>)
 , parse, parsebasic, notused
 , Parser(..), Nat(..), Descr(..), RealParser, Error(..)
 , Symbol, OC(..), SymbolR(..), mk_range
 , usererror
 )
where
-- import IOExts
import UU_BinaryTrees
-- =======================================================================================
-- =====THE INTERFACE ====================================================================
-- =======================================================================================
infixr 2 <<|>
infixl 3 <|> 
infixl 4 <*> 
infixl 5 <..>
-- infixr 4 <?!> 
pSucceed ::                           a                 -> Parser s a
pFail    ::                                                Parser s a
pLRange  :: Symbol s => SymbolR s -> s                  -> Parser s s 
pSym     :: Symbol s => s                               -> Parser s s
(<..>)   :: Symbol s => s -> s                          -> Parser s s
(<|>)    :: Symbol s => Parser s        a -> Parser s a -> Parser s a
(<<|>)   :: Symbol s => Parser s        a -> Parser s a -> Parser s a
(<*>)    :: Symbol s => Parser s (b -> a) -> Parser s b -> Parser s a
parse    :: Symbol s => Parser s a-> [s] -> (a, String)

pSym a          = a <..> a
a <..> b        = pLRange (Range (a,CL) (b,CL)) a

parse p inp
 = let noErrors             = []
       emptyStack           = ()
       noSteps              = []
       handle_eof           = foldr notused ((emptyStack, noErrors), noSteps)
       (res,errors)         = parsebasic p handle_eof inp
   in (res,unlines (map show errors))

parsebasic (Parser _ root@(P rp) _) handle_eof inp
 = let (((res,()),errors),_)= rp handle_eof inp
   in (res, errors)

-- CLASS SYMBOL ---
class (Ord s, Show s) => Symbol s 
-- =======================================================================================
-- ===== LEVEL 1: BASIC PARSING MACHINERY  ========================================================
-- =======================================================================================
newtype RealParser s a = P (forall r .([s] -> Result r s) -> [s] -> Result (a,r) s)
type Result c s= ((c,Errors s),[Int])
succeed  v           = P (\ k -> \inp -> let (~(           r  , m), st) = k inp 
                                         in  (((v   ,      r) , m), st))
fwby  (P p) ~(P q)   = P (\ k -> \inp -> let (~(~(pv,~(qv, r)), m), st) = p (q k) inp 
                                         in  (((pv qv    , r ), m), st)) 
(P p) `fwbye` qv     = P (\ k -> \inp -> let (~(~(pv     , r ), m), st) = p k inp 
                                         in  (((pv qv    , r ), m), st)) 
pv    `efwby` ~(P q) = P (\ k -> \inp -> let (~(~(   qv  , r ), m), st) = q k inp 
                                         in  (((pv qv    , r ), m), st)) 
bestp (P p)  (P q)   = P (\ k -> \inp -> (p k inp `best` q k inp))

addstep   s ~( v       , ss) = (v               , s:map (+s) ss)
 
accept    v ~(~(r,msgs), ss) = (((v,r),    msgs), 0:ss)    
addresult v ~(~(r,msgs), ss) = (((v,r),    msgs),   ss)
addmsg    m ~(~(r,msgs), ss) = ((   r , m: msgs),   ss)
insert   a s  =           addmsg (Insert a s)  .addresult a
delete   b s  =           addmsg (Delete b s)
insertEof  a  =           addmsg (InsertEof a)  .addresult a
deleteEof  b  =           addmsg (DeleteEof b)
notused    b  = failstep   .addmsg (NotUsed b)
failstep      = addstep (5::Int)

best left@(lvm, [])   _                   = left
best _                right@(rvm,[])      = right
best left@(lvm, ll@(l:ls)) right@(rvm, rr@(r:rs))   
  = (if l == 0 then if r == 0 then addstep 0 (best (lvm, ls) (rvm, rs)) 
                              else left
               else if r == 0 then right
                              else (if (ll `beats` rr) 4 then lvm else rvm, zipWith min ll rr))
    where ([l]    `beats` (r:rs)) _ = l <= r
          ((l:ls) `beats` [r]   ) _ = l <= r
          ((l:ls) `beats` (r:rs)) n = (if n == 0 then l <= r  else (ls `beats` rs) (n-1))
          (_      `beats` _     ) n = systemerror "beats"

-- =======================================================================================
-- ===== LEVEL 2: ABSTRACT INTERPREATATION ===============================================
-- =======================================================================================
data Parser     s a= Parser { leng   :: Nat  
                            , p      :: RealParser s a
                            , descr  :: Descr s a
                            }
data Descr s a     = Descr  { emptyd :: Maybe a                             
                            , corr0  :: RealParser s a
                            , corr1  :: RealParser s a
                            , table  :: [(SymbolR s, RealParser s a)]
                            } 
                    | Dyn  
-- =======================================================================================
-- ===== ERROR MESSAGES ==================================================================
-- =======================================================================================
data Error s = Insert s s
             | InsertEof s
             | Delete s s
             | DeleteEof s
             | NotUsed s

type Errors s = [Error s]

instance Show s => Show (Error s ) where
 show (Insert    a s) = " Inserted: "       ++ show a++" before "++ show s
 show (InsertEof a  ) = " Inserted: "       ++ show a++" before eof"
 show (Delete    b s) = " Deleted : "       ++ show b++" before "++show s
 show (DeleteEof b  ) = " Deleted : "       ++ show b++" before eof"
 show (NotUsed   b  ) = " Not used: " ++ show b

-- =======================================================================================
-- ===== LEVEL 2:  COMBINATORS ===========================================================
-- =======================================================================================
pSucceed v  = Parser Zero (succeed v) (Descr (Just v) (succeed v) (succeed v) [])
errorfail   = usererror "calling always failing parser"
pFail       = Parser infinite errorfail (Descr Nothing errorfail errorfail [])
-- <|> -----------------------------------------------------------------------------------
(Parser ll lp ld)  <|> (Parser rl rp rd) 
 = let (len, b) = shortest ll rl
       newdescr = if b then ld `orDescr` rd else rd `orDescr` ld
       newpars  = case newdescr of
                  Dyn           -> if b then lp `bestp` rp else rp `bestp` lp
                  Descr _ _ _ _ -> mkParser newdescr
   in Parser len newpars newdescr

Dyn                   `orDescr` _   = Dyn
_                     `orDescr` Dyn = Dyn
(Descr le lc0 lc1 lt) `orDescr` (Descr re rc0 rc1 rt)
 = let orempty Nothing (Just v) = systemerror "left longer in orDescr"
       orempty le      Nothing  = le
       orempty _       _        = usererror "two alternatives can both recognise the empty sequence"
       resempty           = orempty le re
   in if      null lt then (Descr resempty  rc0  rc1 rt)
      else if null rt then (Descr resempty  lc0  lc1 lt)
      else                 (Descr resempty (lc0 `bestp` rc0) (lc1 `bestp` rc1) (merge_tables lt rt False))

-- <*> -----------------------------------------------------------------------------------
(Parser ll lp ld) <*> ~(Parser rl rp rd) 
 = let len = addnat ll rl
       (newpars, newdescr) = (ld, lp) `seqDescr` (rd, rp)
   in  Parser len newpars newdescr

(Dyn                      , lp) `seqDescr` (_, rp)  = ( lp `fwby` rp, Dyn)
(Descr Nothing  lc0 lc1 lt, lp) `seqDescr` (_, rp)  = ( lp `fwby` rp
                                                      , Descr Nothing  (lc0 `fwby` rp ) (lc1 `fwby` rp) 
                                                              (mapsnd (`fwby` rp) lt)
                                                      )
(Descr (Just v) lc0 lc1 lt, lp) `seqDescr` (rd, rp)
 =  case rd of
    Dyn                 -> ( if null lt then v `efwby` rp else lp `fwby` rp, Dyn)
    Descr re rc0 rc1 rt -> let newdescr = Descr (case re of {Nothing -> Nothing; Just w -> Just (v w)}) 
                                               (lc0 `fwby` rc0) 
                                               (lc1 `fwby` rp) 
                                               (merge_tables (mapsnd (  `fwby` rp) lt)
                                                             (mapsnd (v `efwby`  ) rt)
                                                              False
                                               )
                           in ( if null lt then v `efwby` rp else mkParser newdescr
                              , newdescr
                              )
_ `seqDescr` _ =   systemerror "seqDescr"                                                                 

-- =======================================================================================
-- ===== ELEMENTARY PARSER  ==============================================================
-- =======================================================================================
pLRange EmptyR _ = pFail
pLRange range ins_sym 
  = let cmp  = cmpSR range
        corr1 = P (\ k -> \ inp@(s:ss) -> if null ss 
                                          then deleteEof s (unP rp k ss)  
                                               `best` 
                                               insertEof s (k inp)
                                          else delete s (head ss) (unP rp k ss)  
                                               `best` 
                                               insert ins_sym s (k inp))

        corr0 = P (\ k -> \ inp         -> insertEof ins_sym  (k inp))
        pr    = P (\ k -> \ inp@(s:ss)  -> accept s (k ss))
        descr = Descr Nothing corr0 corr1 [(range, pr)]
        rp = mkParser descr
    in Parser one rp descr

unP (P p) = p
-- =======================================================================================
-- ===== MKPARSER ========================================================================
-- =======================================================================================
mkParser :: Symbol s => Descr s a-> RealParser s a
mkParser (Descr ed (P corr0) (P corr1) chcs) 
 = let locfind = case chcs of
                 [(ran, pp)] -> let comp = cmpSR ran 
                                in \ s -> if comp s == EQ then Just pp else Nothing
                 _           -> btFind cmpSR.tab2tree $ chcs
       parse1  = P (\k inp -> case inp of
                              (s:ss) ->  case locfind s 
                                         of Just (P p) -> p k inp
                                            Nothing    -> failstep (corr1 k inp)
                              []     ->  failstep ( corr0  k [])
                   )
    in case ed of
       Nothing  -> parse1
       Just v   -> if null chcs then succeed v 
                                else succeed v `bestp` parse1
-- =======================================================================================
-- ===== CHOICE STRUCTURES   =============================================================
-- =======================================================================================
merge_tables l [] _ = l
merge_tables [] r _ = r
merge_tables lss@(l@(le@(Range a b),ct ):ls) rss@(r@(re@(Range c d),ct'):rs) swapped
 = let ct'' = if swapped then ct' `bestp`  ct else ct `bestp` ct'
   in
    if      c<a then   merge_tables rss lss (not swapped)    -- swap                
    else if b<c then l:merge_tables ls  rss swapped          -- disjoint case
    else if a<c then (Range a (before c),ct) :merge_tables ((Range c b,ct):ls)          rss swapped
    else if b<d then (Range a b,ct'')        :merge_tables ((Range (after b) d,ct'):rs) ls  (not swapped)
    else if b>d then merge_tables rss lss (not swapped)
                else (le,ct'') : merge_tables ls rs swapped-- equals
-- =======================================================================================
-- ===== SYMBOL RANGES ===================================================================
-- =======================================================================================
data OC = RO | CL | LO
        deriving (Eq,Ord)

instance Show OC where
   show RO = "("
   show LO = ")"
   show CL = "|"

type Point s = (s,OC)

data  SymbolR s  =  Range (Point s) (Point s) | EmptyR deriving (Eq,Ord)

mk_range :: Ord a => (a,OC) -> (a,OC) -> SymbolR a
mk_range l@(lv, lb) r@(rv,rb) = if lv > rv then EmptyR else Range l r

instance (Symbol s) => Show (SymbolR s) where
  show (Range (a, CL) (b,CL)) = if a==b then "|"++show a++"|"
                        else "|"++show a++ ".." ++ show b ++ "|"
  show (Range (a, ab) (b, bb)) = show ab++ show a ++ ".." ++ show b ++ show bb
  show EmptyR       = " || "
                              

before (v,CL) = (v,RO)
before (v,LO) = (v,CL)
before (v,RO) = systemerror ("before RO for value "++ show v ++" found.")
after  (v,CL) = (v,LO)
after  (v,RO) = (v,CL)
after  (v,LO) = systemerror ("after LO for value "++ show v ++" found.")

cmpSR (Range (l,lb) (r,rb))  
  = if l == r && lb == CL && rb == CL then compare l
    else \s -> case compare l s of
               LT -> case compare r s of
                     EQ -> compare rb CL
                     LT -> LT
                     GT -> EQ
               EQ -> compare lb CL
               GT -> GT
-- =======================================================================================
-- ===== MINIMAL LENGTHS (lazily formulated) =============================================
-- =======================================================================================
data Nat = Zero
         | Succ Nat
         deriving (Eq, Show) 

length_le Zero       _      = True 
length_le (Succ l)  r = case r of
                        Zero     -> False
                        Succ r'  -> length_le l r'

shortest Zero      _ = (Zero, True) 
shortest (Succ l)  r = case r of 
                       Zero   -> (Zero, False)
                       Succ r -> let (s, b) = shortest l r in (Succ s, b) 

addnat Zero     r = r
addnat (Succ l) r = Succ (addnat l r)

one      = Succ Zero
infinite = Succ infinite

-- <<|> -----------------------------------------------------------------------------------
(Parser ll lp _) <<|> (Parser rl rp _)  
 = let (s , b) = shortest ll rl 
       priopars (P pp) (P pq)  = P( \k inp 
                                    -> let pres = pp k inp
                                           qres = pq k inp
                                       in if       proceeds pres then pres
                                           else if proceeds qres then qres
                                           else    if b then pres `best` qres else qres `best` pres 
                                  )
   in Parser s (priopars lp rp) Dyn

proceeds (_,(0:_)) = True
proceeds (_,l    ) = null l
mapsnd f t = [(s, f e) | (s, e) <- t]
-- =======================================================================================
-- ===== TRACING  and ERRORS  ============================================================
-- =======================================================================================
{-
notrace m v  = v
yestrace m v = trace ("\n"++show m) v


yesshow l r v = trace ("\nleft: "++show l ++ " right: "++ show r) v
noshow  l r v =                                                   v
-}

usererror   m = error ("Your grammar contains a problem.\n" ++ m) 
systemerror m = error ("I apologise: I made a mistake in my design. This should not have happened.\n"
                       ++
                       " Please report: " ++ m ++ " to doaitse@cs.uu.nl\n")