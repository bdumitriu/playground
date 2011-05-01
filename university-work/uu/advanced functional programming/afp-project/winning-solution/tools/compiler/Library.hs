module Library (makeProgram,makeDirFrags,
		Program,compile,
		Opts (..),allOpts,noOpts,compile',
                Fragment,LabelledFragment,
                (.:.),goto,
                alwaysft,invertft,ifthen,
                (.*.),(*.),forever,rep,skip,
		(.>:.),(.>*.),jump,
                label,fresh,freshLabel,Label(..),
                cmdSense,cmdMark,cmdUnmark,cmdPickUp,
                cmdDrop,cmdTurn,cmdMove,cmdFlip,cmdWasteTime,
                module Language,
                produceCommandList,numberCommands,inlineGotos,
                instantiate,makecode,getNumbering,
               ) where

import qualified Data.FiniteMap as FM
import Debug.Trace
import Data.FiniteMap (FiniteMap)
import Data.Set
import Data.List
import Data.Maybe
import Control.Monad

import Debug.Trace

import Language

data AntState=AntState { genNum :: Int,
		         renamed :: Set String }
			 

newtype StateM s v = StateM { unST :: s -> (v,s) }

instance Monad (StateM s) where
   return v = StateM { unST = \s -> (v,s) }
   m >>= f = StateM { unST = \s -> let (v,s') = unST m s in unST (f v) s' }

runStateM :: StateM s v -> s -> v
runStateM m s = fst $ (unST m) s

getStateM :: StateM s s
getStateM = StateM { unST = \s -> (s,s) }

putStateM :: s -> StateM s ()
putStateM s = StateM { unST = \_ -> ((),s) }

newtype Label = Label { unLabel :: String }
 deriving (Eq,Ord)

instance Show Label where
 show = show.unLabel

class HasLabel l where
  labOf :: l -> Label

instance HasLabel Label where
  labOf l = l

type Ant = StateM AntState

freshLabel :: Ant Label
freshLabel = StateM { unST = (\s -> let n = genNum s
			            in (Label ("gen$"++show n),
					s { genNum = n+1 })) }

freshHintedLabel :: String -> Ant Label
freshHintedLabel str = StateM { unST = (\s -> let n = genNum s
                                                  seen = renamed s
                                              in if str `elementOf` seen 
       					           then (Label (str++"$"++show n),
			  			         s { genNum = n+1 })
				                   else (Label str, 
							 s {renamed = addToSet seen str}))
			      }

type Fragment = Ant Fragment'
data Instr = Cmd (Command' Label)
           | Goto Label String
  deriving Show

doGoto :: Label -> Instr
doGoto lab = Goto lab ""

mapInstr :: (Label -> Label) -> Instr -> Instr
mapInstr f (Cmd c) = Cmd (mapCommand' f c)
mapInstr f (Goto lab s) = Goto (f lab) s

mapMInstr :: Monad m => (Label -> m Label) -> Instr -> m Instr
mapMInstr f (Cmd c) = do c' <- mapMCommand' f c
                         return $ Cmd c'
mapMInstr f (Goto lab s) = do lab' <- f lab
                              return $ Goto lab' s

type FM = FiniteMap Label Instr

data Fragment' = Fragment' 
    {
     fmF :: FM,
     startF :: Label,
     endF :: Maybe Label
    }
  deriving Show

findinterestinglabels :: Fragment' -> [String]
findinterestinglabels frag
 = filter interesting (map unLabel labs)
     where labs = [startF frag] ++ end ++ FM.keysFM (fmF frag)
           end = case endF frag of
                    Nothing -> []
                    Just l -> [l]

interesting :: String -> Bool
interesting ('g':'e':'n':'$':_) = False
interesting _ = True

showinterestinglabels :: Fragment' -> String
showinterestinglabels frag = sepBy "," (findinterestinglabels frag)

sepBy :: String -> [String] -> String
sepBy _ [] = ""
sepBy _ [x] = x
sepBy sep (x:x':xs) = x ++ sep ++ sepBy sep (x':xs)

instance (Show key,Show elt) => Show (FiniteMap key elt) where
  show fm = show (FM.fmToList fm)

{-
-- might not use this...
check' :: Fragment' -> Fragment'
check' f = if size > 10000 then error "program too large"
	   else trace ("program size: "++show size) f
          where size = FM.sizeFM (fmF f)
-}

-- can't easily tell *what* label unless we don't use plusFM_C
-- or add a conflict option and postprocess for it
conflict :: Label -> a -> b -> c
conflict (Label lab) _ _ = error ("duplicate label "++lab)

plusFM :: FM -> FM -> FM
plusFM fm1 fm2 = FM.foldFM (\lab i fm -> addToFM fm lab i) fm1 fm2

addToFM :: FM -> Label -> Instr -> FM
addToFM fm lab instr = FM.addToFM_C (conflict lab) fm lab instr

skip :: Fragment
skip = do start <- freshLabel
          end <- freshLabel
          return $ Fragment' 
                     { fmF = FM.unitFM start (doGoto end), 
                       startF =start, endF =Just end }

-- sequencing
(.*.) :: Fragment -> Fragment -> Fragment
f .*. g = do f' <- f
             g' <- g
             let mergedFM = plusFM (fmF f') (fmF g')
             let endf' = case endF f' of
                    Just l -> l
                    Nothing -> error 
                     ("trying to put something after the end of a "
                       ++"non-fallthrough piece of code "
		       ++showinterestinglabels f'++".")
             let resultFM = addToFM mergedFM (endf') (doGoto (startF g'))
             return $ Fragment' 
                       { fmF = resultFM, startF = startF f', endF = endF g' }

-- combine without sequencing
(*.) :: Fragment -> Fragment -> Fragment
f *. g = do f' <- f
            g' <- g
            let mergedFM = plusFM (fmF f') (fmF g')
            case endF f' of
                   Just _ -> error 
                             ("first fragment for *. has a fallthrough label "++
			      showinterestinglabels f')
                   Nothing -> return ()
            return $ Fragment' 
                       { fmF = mergedFM, startF = startF f', endF = endF g' }


goto :: HasLabel l => l -> Fragment
goto lab = do start <- freshLabel
              let lab' = labOf lab
              return $ Fragment' 
                         { fmF = FM.unitFM start $ doGoto lab',
                           startF = start,
                           endF = Nothing }

data LabelledFragment = LF {getlabel :: Label,
                            getcode :: Fragment}


instance HasLabel LabelledFragment where
  labOf l = getlabel l


infixl 2 .:.
infixr 3 .*.

{-
ulc :: Fragment -> LabelledFragment
ulc f = LF { unLF = do lab <- freshLabel
                       code' <- label (unLabel lab) f
                       return $ LF' { getlabel' = lab,getcode' = code' } }
-}


(.:.) :: String -> Fragment -> LabelledFragment
(.:.) = labelledcode

labelledcode :: String -> Fragment -> LabelledFragment
labelledcode lab f = let code' = label lab f 
                     in LF { getlabel = Label lab,
                             getcode = code' }

label :: String -> Fragment -> Fragment
label lab f = 
              do f' <- f
                 let resultFM = addToFM (fmF f') (Label lab) 
                                                 (doGoto (startF f'))
                 return $ Fragment' { fmF = resultFM,
                                      startF = Label lab,
                                      endF = endF f' }

infix 2 .>:.
infixr 3 .>*.

(.>:.) :: String -> [Fragment] -> LabelledFragment
str .>:. frags = str .:. foldr1 (.*.)
		           (map (\(n,frag)
                                    -> label (str++":"++show n) frag)
			        (zip ([0..(length frags-2)]::[Int]) frags)
			    ++ [last frags])

(.>*.) :: Fragment -> [Fragment] -> [Fragment]
(.>*.) = (:)


jump :: HasLabel l => l -> [Fragment]
jump l = [goto l]

fresh :: String -> Fragment -> Fragment
fresh hint f =
               do f' <- f
                  lab <- freshHintedLabel hint
                  let resultFM = addToFM (fmF f') lab (doGoto (startF f'))
                  return $ Fragment' { fmF = resultFM,
                                      startF = lab,
                                      endF = endF f' }

cmd :: UCommand' Label -> Instr
cmd = Cmd . noComment                

-- make something that takes a non-fall through argument take a fall-through
-- argument that just goes to the same end point
ifthen :: (Fragment -> Fragment) -> Fragment -> Fragment
ifthen func ft = do end <- freshLabel
                    func (ft .*. goto end) .*. label (unLabel end) skip

alwaysft :: (Fragment -> Fragment) -> Fragment
alwaysft func = do end <- freshLabel
		   res <- func (goto end) .*. goto end
	           return $ Fragment'
			  { fmF = fmF res,
			    startF = startF res,
			    endF = Just end }

invertft :: (Fragment -> Fragment) -> (Fragment -> Fragment)
invertft func newrun = do newrun' <- newrun
		          case endF newrun' of
			     Nothing -> return ()
			     Just _ -> error 
				       ("first fragment for invertft(...) must "++
				        "not fall through "++
					showinterestinglabels newrun')
                          run <- freshLabel
   	                  func (goto run) .*. newrun *. 
                               label (unLabel run) skip


-- falls through if condition is not true
cmdSense :: SenseDir -> Cond -> Fragment -> Fragment
cmdSense dir cond succeed
  = do start' <- freshLabel
       end' <- freshLabel
       succeed' <- succeed
       case endF succeed' of 
         Nothing -> return ()
         Just _ -> error ("first fragment for cmdSense must not fall through "++
			  showinterestinglabels succeed')
       return $ Fragment' 
                  { fmF = addToFM (fmF succeed') start' $ cmd $
                                Sense dir (startF succeed') end' cond,
                    startF = start',
                    endF = Just end' }

cmdMark :: MarkID -> Fragment
cmdMark num = do start <- freshLabel
                 end <- freshLabel
                 return $ Fragment' 
                            { fmF = FM.unitFM start $ cmd $ Mark num end,
                              startF = start,
                              endF = Just end }

cmdUnmark :: MarkID -> Fragment
cmdUnmark num = do start <- freshLabel
                   end <- freshLabel
                   return $ Fragment' 
                              { fmF = FM.unitFM start $ cmd $ Unmark num end,
                                startF = start,
                                endF = Just end }

-- falls through if no food to pickup
cmdPickUp :: Fragment -> Fragment
cmdPickUp succeed
  = do start' <- freshLabel
       end' <- freshLabel
       succeed' <- succeed
       case endF succeed' of 
         Nothing -> return ()
         Just _ -> error ("first fragment for cmdPickUp must not fall through "++
			  showinterestinglabels succeed')
       return $ Fragment' 
                  { fmF = addToFM (fmF succeed') start' $ cmd $ 
                                   PickUp (startF succeed') end',
                    startF = start',
                    endF = Just end' }

cmdDrop :: Fragment
cmdDrop = do start <- freshLabel
             end <- freshLabel
             return $ Fragment' { fmF = FM.unitFM start $ cmd $ Drop end,
                                  startF = start,
                                  endF = Just end }

cmdTurn :: TurnDir -> Fragment
cmdTurn dir = do start <- freshLabel
                 end <- freshLabel
                 return $ Fragment' 
                            { fmF = FM.unitFM start $ cmd $ Turn dir end,
                              startF = start,
                              endF = Just end }

cmdWasteTime :: Fragment
cmdWasteTime = alwaysft (cmdSense Here Foe)

-- falls through if move succeeds
cmdMove :: Fragment -> Fragment
cmdMove failf
  = do start' <- freshLabel
       end' <- freshLabel
       fail' <- failf
       case endF fail' of 
         Nothing -> return ()
         Just _ -> error ("first fragment for cmdMove must not fall through "++
			  showinterestinglabels fail')
       return $ Fragment' 
                  { fmF = addToFM (fmF fail') start' $ cmd $ 
                                  Move end' (startF fail'),
                    startF = start',
                    endF = Just end' }

-- falls through if flip fails
cmdFlip :: Int -> Fragment -> Fragment
cmdFlip num succeed
  = do start' <- freshLabel
       end' <- freshLabel
       succeed' <- succeed
       case endF succeed' of 
         Nothing -> return ()
         Just _ -> error ("first fragment for cmdFlip must not fall through "++
			  showinterestinglabels succeed')
       return $ Fragment' 
                  { fmF = addToFM (fmF succeed') start' $ cmd $ 
                                  Flip num (startF succeed') end',
                    startF = start',
                    endF = Just end' }

forever :: Fragment -> Fragment
forever f = do f' <- f
               let endf' = case endF f' of
                    Just l -> l
                    Nothing -> error 
                     ("trying to make a forever loop from something that"
                        ++" doesn't fall through "++
		      showinterestinglabels f')
               let resultFM = addToFM (fmF f') (endf') (doGoto (startF f'))
               return $ Fragment' 
                          { fmF = resultFM, 
                            startF = startF f', 
                            endF = Nothing
                          }

rep :: Int -> Fragment -> Fragment
rep 0 _    = skip -- for cases where we call rep with variable args
rep n cod  = do code' <- cod
                rens <- makeRenaming (FM.keysFM (fmF code'))
                foldr1 (.*.) $ map (makecopy rens) [1..n]
       where 
             makeRenaming :: [Label] -> Ant (FiniteMap (Label,Int) Label)
             makeRenaming labs = myfoldM makeRen (return FM.emptyFM) labs
             makeRen :: Label -> FiniteMap (Label,Int) Label
                     -> Ant (FiniteMap (Label,Int) Label)
             makeRen lab fm = myfoldM (makeR lab) (return fm) [1..n]

             makeR :: Label -> Int -> FiniteMap (Label,Int) Label
                   -> Ant (FiniteMap (Label,Int) Label)

             makeR lab k fm = do newlab <- freshHintedLabel (unLabel lab)
                                 return (FM.addToFM fm (lab,k) newlab)

{-
             rename fm k lab = if FM.elemFM lab fm 
                                then (if k==1 then lab 
                                      else Label (unLabel lab++"$"++show k))
                                else lab
-}

             rename rens k lab = case FM.lookupFM rens (lab,k) of
                                   Nothing -> lab
                                   Just lab' -> lab'

             makecopy rens k = relabel (rename rens k) cod

foldMFM :: Monad m => (key -> elt -> a -> m a) -> m a -> FiniteMap key elt -> m a
foldMFM func base fm = myfoldM (uncurry func) base (FM.fmToList fm)

myfoldM _ e [] = e
myfoldM f e (x:xs) = do a <- myfoldM f e xs
                        f x a

relabel :: (Label -> Label) -> Fragment -> Fragment
relabel f cod  = do code' <- cod
                    let newfm = FM.foldFM add FM.emptyFM (fmF code')
                    let start' = f (startF code')
                    mend' <- case endF code' of
                              Nothing -> return Nothing
                              Just end -> do let end' = f end
                                             return $ Just end'
                    return $ Fragment' 
                             { fmF = newfm,
                               startF = start',
                               endF = mend' }
    where add lab instr fm = FM.addToFM fm (f lab) (mapInstr f instr)

data Program' = Program' { startlabel :: Label,
                           code :: [LabelledFragment] }

type Program = Ant Program'

makeProgram :: HasLabel l => l -> [LabelledFragment] -> Program
makeProgram lab frags = do let lab' = labOf lab
                           return $ Program' 
                                      { startlabel = lab', code = frags }


compile :: Program -> String
compile = compile' allOpts

compile' :: Opts -> Program -> String
compile' opts
   = outputCommands . produceCommandList . commentStates
     . numberCommands . showStates 
     . doopts opts . showStates
     . inlineGotos
  -- . listRealLabels 
     . commentLabels
     . instantiate . makecode

makecode :: Program -> Fragment
makecode p = do p' <- p
                goto (startlabel p') *. foldr1 (*.) (map getcode (code p'))


commentLabels :: Fragment' -> Fragment'
commentLabels f = Fragment' {startF = startF f,
			     endF = endF f,
			     fmF = foldr doLabel (fmF f) (FM.keysFM (fmF f))}

      where followGoto lab = case FM.lookupFM (fmF f) lab of
			      Just (Goto lab' _) -> followGoto lab'
			      Just (Cmd _) -> lab
			      Nothing -> error 
					 ("label "++unLabel lab++" not found "
					  ++"in commentLabels")

            doLabel lab fm = if ignore lab then fm
                             else let lab' = followGoto lab
                                      Just i = FM.lookupFM fm lab'
                                  in FM.addToFM fm lab' (addLabel lab i)

            addLabel lab i = case i of
                               Cmd c
                                   -> Cmd $ addCommentEnd (unLabel lab) c
                               _ -> i
            ignore lab = take 4 (unLabel lab)=="gen$"


instantiate :: Fragment -> Fragment'
instantiate f = let res = runStateM f (AntState { genNum = 0, renamed = emptySet })
                in case (endF res) of
                     Nothing -> res
                     Just _ -> error 
                               "argument to compile has a fallthrough label"

seqString :: ([a] -> b) -> [a] -> b
seqString f [] = f []
seqString f (s:ss) = seqString (\ss' -> f $! (s:ss')) ss

showStates :: Inlined -> Inlined
showStates i = seqString trace ("program has "++show (states i)++" states") i

listRealLabels :: Fragment' -> Fragment'
listRealLabels f = trace ("real labels:\n" ++ disp (filter interesting (map unLabel (FM.keysFM (fmF f))))) f
  where disp ls = sepBy "\n" ls

states :: Inlined -> Int
states i = length (FM.keysFM (fmI i))

data Opts = Opts { cse :: Bool,
		   collapsebranches :: Bool,
                   cfmarks :: Bool,
                   peephole :: Bool
		 }

allOpts :: Opts
allOpts = Opts { cse = True, collapsebranches = True, cfmarks = False,
	         peephole = True }

noOpts :: Opts
noOpts = Opts { cse = False, collapsebranches = False, cfmarks = False,
	        peephole = True }


doopts :: Opts -> Inlined -> Inlined
doopts opts = enableIf cfmarks optimizemarks . \i ->
                let oldnum = states i
                    optsuite = enableIf cse cseonce .
                               enableIf collapsebranches docollapsebranches .
                               enableIf peephole dopeephole 
                    i' = optsuite i
                    newnum = states i'
          in if newnum<oldnum then doopts opts i' else i

  where enableIf cond opt = if cond opts then inlineGotos.opt else id

dopeephole :: Inlined -> Fragment'
dopeephole i 
  = Fragment' { fmF = res, endF = Nothing, startF = startI i }
    where res = FM.foldFM doOne FM.emptyFM (fmI i) 
          doOne lab c fm = FM.addToFM fm lab (process lab c)

          getucmd lab = fst (unC (fromJust (FM.lookupFM (fmI i) lab)))
          getcomment lab = snd (unC ( fromJust (FM.lookupFM (fmI i) lab)))

          process _ (C (Sense Here lab1 lab2 (Marker n),comment))
            | getucmd lab2 == Mark n lab1 
                = Cmd (C (Mark n lab1,comment++" "++getcomment lab2))
            | getucmd lab1 == Unmark n lab2
                = Cmd (C (Unmark n lab2,comment++" "++getcomment lab2))
          process _ c = Cmd c

docollapsebranches :: Inlined -> Fragment'
docollapsebranches i 
  = Fragment' { fmF = res, endF = Nothing, startF = startI i }
    where res = FM.foldFM doOne FM.emptyFM (fmI i) 
          doOne lab c fm = FM.addToFM fm lab (process lab c)

          process _ c@(C (Sense Here _ _ Foe,_))
             = Cmd c -- explicit timewaste
          process lab (C (Sense _ lab1 lab2 _,comment)) 
                | lab1==lab2 && lab/=lab1 
             = Goto lab1 comment
          process lab (C (Flip _ lab1 lab2,comment))
                | lab1==lab2 && lab/=lab1
             = Goto lab1 comment
          process _ c = Cmd c      

data MarkKnown = Unknown | KnownOn | KnownOff
  deriving (Eq,Show)
mergemarkknown :: MarkKnown -> MarkKnown -> MarkKnown

mergemarkknown m m' | m==m' = m
mergemarkknown _ _ = Unknown

type MarksKnown = [MarkKnown]
mergemarksknown :: MarksKnown -> MarksKnown -> (MarksKnown,Bool)
mergemarksknown ms ms' = let ms'' = map (uncurry mergemarkknown) (zip ms ms')
                         in if ms'' == ms then (ms,False) else (ms'',True)

allMarksUnknown :: MarksKnown
allMarksUnknown = replicate 6 Unknown

optimizemarks :: Inlined -> Fragment'
optimizemarks i 
  = Fragment' { fmF = res, endF = Nothing, startF = startI i }
       where cf = markscontrolflow i
             res = FM.foldFM doOne FM.emptyFM (fmI i)

             getMarkAt n lab = case FM.lookupFM cf lab of
                                 Nothing -> Unknown -- dead code?
                                 Just known -> trace (show (known!!n)) (known!!n)

             doOne lab c fm = FM.addToFM fm lab (process lab c)

             process lab (C ((Mark n lab1),comment))
                | getMarkAt n lab == KnownOn = Goto lab1 comment
             process lab (C ((Unmark n lab1),comment))
                | getMarkAt n lab == KnownOff = Goto lab1 comment
             process lab (C (Sense Here lab1 lab2 (Marker n),comment))
                | mark == KnownOn = Goto lab1 comment
                | mark == KnownOff = Goto lab2 comment
                    where mark = getMarkAt n lab
             process _ c = Cmd c

markscontrolflow :: Inlined -> FiniteMap Label MarksKnown
markscontrolflow i = doIterate FM.emptyFM

  where 
--        allUnknown = FM.mapFM (\_ _ -> allMarksUnknown) (fmI i)

        doIterate fm = let (fm',changed) = iterateOnce fm
                       in if changed then doIterate fm else fm'
        iterateOnce fm = FM.foldFM doOne (fm,False) (fmI i)

        doOne lab (C (c,_)) fmc = doOne' fmc lab c
        doOne' fmc _ (Move _ lab2) = update lab2 allMarksUnknown fmc
        doOne' fmc lab (Turn _ lab1) = propagatefrom lab lab1 fmc
        doOne' fmc lab (Flip _ lab1 lab2) = propagatefrom lab lab1 
                                             (propagatefrom lab lab2 fmc)
        doOne' fmc lab (Drop lab1) = propagatefrom lab lab1 fmc
        doOne' fmc lab (PickUp lab1 lab2) = propagatefrom lab lab1 
                                             (propagatefrom lab lab2 fmc)
        doOne' fmc lab (Mark n lab1) = propagateupd lab lab1 n KnownOn fmc
        doOne' fmc lab (Unmark n lab1) = propagateupd lab lab1 n KnownOff fmc
        doOne' fmc lab (Sense Here lab1 lab2 (Marker n))
          = propagateupd lab lab1 n KnownOn
              (propagateupd lab lab2 n KnownOn fmc)
        doOne' fmc lab (Sense _ lab1 lab2 _)
          = propagatefrom lab lab1 (propagatefrom lab lab2 fmc)

        update lab newmarks (fm,changed) 
          = case FM.lookupFM fm lab of
              Nothing -> (FM.addToFM fm lab newmarks,True)
              Just oldmarks -> 
                let (finalmarks,changed')=mergemarksknown oldmarks newmarks
                in (FM.addToFM fm lab finalmarks,changed || changed')

        propagatefrom fromlab tolab fmc
          = case FM.lookupFM (fst fmc) fromlab of
              Nothing -> fmc
              Just propmarks -> update tolab propmarks fmc

        propagateupd fromlab tolab pos known fmc
          = case FM.lookupFM (fst fmc) fromlab of
              Nothing -> fmc
              Just propmarks -> update tolab (change propmarks pos known) fmc

        change marks n val = take n marks ++ [val] ++ drop (n+1) marks

cseonce :: Inlined -> Fragment'
cseonce i = Fragment' { fmF = res, endF = Nothing, startF = startI i }
             where
                (res,_) = FM.foldFM doOne (FM.emptyFM,FM.emptyFM) (fmI i)
                doOne lab c (res',instrs) 
                  = case FM.lookupFM instrs c of
                      Nothing -> (FM.addToFM res' lab (Cmd c),
				  FM.addToFM instrs c lab)
                      Just lab' -> (FM.addToFM res' lab 
                                                    (Goto lab' (getComment c)),
				    instrs)


-- don't care about the end label once the fragment is "finished";
-- we have nowhere sensible to fix it up to anyway so if it doesn't
-- get fixed up then there will just be an error

data Inlined = Inlined { fmI :: FiniteMap Label (Command' Label)
                       , startI :: Label }
     deriving Show

inlineGotos :: Fragment' -> Inlined
inlineGotos = inlineGotos' . pushGotoComments

pushGotoComments :: Fragment' -> Fragment'
pushGotoComments frag 
  = Fragment' { startF = startF frag,
       		endF = endF frag,
		fmF = newfm }
    where newfm = foldr pushOne (fmF frag) (FM.fmToList (fmF frag))
          pushOne (_,Cmd _) fm = fm
          pushOne (lab,Goto targ str) fm 
           = let lab' = followGoto lab
                 Cmd c = fromJust (FM.lookupFM (fmF frag) lab')
                 fm' = FM.addToFM fm lab (Goto targ "")
             in FM.addToFM fm' lab' (Cmd (addCommentEnd str c))

          followGoto lab = case FM.lookupFM (fmF frag) lab of
                               Just (Goto lab' _) -> followGoto lab'
                               Just (Cmd _) -> lab
                               Nothing -> error 
                                          ("label "++unLabel lab++" not found "
					   ++"in pushGotoComments")

inlineGotos' :: Fragment' -> Inlined
inlineGotos' f = Inlined { fmI = FM.foldFM add FM.emptyFM (fmF f),
                          startI = followGoto (startF f) }

      where 
            followGoto lab = case FM.lookupFM (fmF f) lab of
                               Just (Goto lab' _) -> followGoto lab'
                               Just (Cmd _) -> lab
                               Nothing -> error 
                                          ("label "++unLabel lab++" not found "
					   ++"in inlineGotos")

            add lab (Cmd c) fm = FM.addToFM fm lab (mapCommand' followGoto c)
            add _ (Goto _ _) fm = fm

commentStates :: NumberedCommands -> NumberedCommands
commentStates nc = NC { count = count nc,
			fmNC = FM.mapFM 
			         (\num c
                                    -> addCommentBegin ("State "++show num) c)
                                 (fmNC nc) }

produceCommandList :: NumberedCommands -> [Command]
produceCommandList nc = map getcommand [0..(count nc-1)]
       where getcommand num = case FM.lookupFM (fmNC nc) num of
               Just c -> c
               Nothing -> error ("command "++show num++" not found")
           

data NumberedCommands = NC { count :: CommandIndex,
                             fmNC :: FiniteMap CommandIndex Command }
  deriving Show

numberCommands :: Inlined -> NumberedCommands
numberCommands i 
    = if totnum>10000 then error ("that took "++show totnum++" states")
        else trace ("that took "++show totnum++" states") $
	     NC { count = totnum, fmNC = FM.foldFM add FM.emptyFM (fmI i) }
          where (totnum,numbering) = getNumbering i
                getindex lab = case FM.lookupFM numbering lab of
                                Just num -> Just num
                                Nothing -> trace
                                  (unLabel lab++": "++
                                   show (fromJust (FM.lookupFM (fmI i) lab))++
                                   " not found (dead code?)") $ Nothing
                add lab c fm
                 = case getindex lab of 
                     Just num -> FM.addToFM fm num
                                   (mapCommand' (fromJust.getindex) c)
                     Nothing -> fm

data Numbering = Numbering { nextnum :: CommandIndex,
                             labels :: FiniteMap Label CommandIndex,
                             worklist :: [Label] }
  deriving Show

getNumbering :: Inlined -> (CommandIndex,FiniteMap Label CommandIndex)
getNumbering i = (nextnum finalNumbering,labels finalNumbering)
    where finalNumbering = runStateM (doNumbering >> getStateM) initNumbering
          initNumbering = Numbering { nextnum = 0, 
                                      labels = FM.emptyFM,
                                      worklist = [startI i] }
          doNumbering 
              = do st <- getStateM
                   case worklist st of
                     [] -> return ()
                     (lab:labs) 
                       -> do putStateM $ Numbering 
                                    { nextnum = nextnum st+1,
                                      labels = FM.addToFM (labels st) 
                                                          lab
                                                          (nextnum st),
                                      worklist = labs }
                             case FM.lookupFM (fmI i) lab of
                               Just c -> do mapMCommand' visit c
                                            doNumbering
                               Nothing -> error 
                                          ("label "++unLabel lab++" not found "
					   ++" in getNumbering")

          visit lab = do st <- getStateM
                         if FM.elemFM lab (labels st) || 
                            elemIndex lab (worklist st) /= Nothing
                          then return ()
                          else putStateM $ Numbering 
                                   { nextnum = nextnum st,
                                     labels = labels st,
                                     worklist = lab:worklist st }

makeDirFrags :: [Int -> LabelledFragment] -> [LabelledFragment]
makeDirFrags [] = []
makeDirFrags (f:fs) = map f [0..5] ++ makeDirFrags fs
