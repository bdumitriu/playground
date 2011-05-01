{-# LINE 12 "SLUnification.lhs" #-}
module SLUnification where

import SLTypes
import UU.Pretty

type Subst = [(VarName,Type)]

emptysubst = []

(|=>) :: Subst -> Type -> Type
(|=>) subst t = case t of
  (TAny)          ->  TAny
  (TInt)          ->  TInt
  (TBool)         ->  TBool
  (TUnit)         ->  TUnit
  (TFunc a b)     ->  TFunc (subst  |=>  a) (subst  |=>  b)
  (TForall i t')  ->  TForall i (subst  |=>  t')
  (TBound _)      ->  t
  (TVar tvar)     ->  case lookup tvar subst of
                        Just t'  -> t'
                        Nothing  -> t
  (TError s)      ->  TError s

(|||) :: Subst -> Subst -> Subst
(|||) s1 s2 = s1 ++ [(v,s1  |=>  t) | (v,t) <- s2]

unify :: Type -> Type -> (Subst, Type)
unify  TAny         _              =  ([],TAny)
unify  _            TAny           =  ([],TAny)
unify  (TError s)   _              =  ([],TError s) 
unify  _            (TError s)     =  ([],TError s) 
unify  (TVar l)     (TVar r)       =  if l == r
                                        then ([], TVar r)
                                        else ([(l,TVar r)], TVar r)
unify  (TVar l)     t              =  varBind l t
unify  t            (TVar r)       =  varBind r t
unify  TBool        TBool          =  ([], TBool)
unify  TBool        t              =  ([], TError ("Bool /= " ++ show t))
unify  t            TBool          =  ([], TError ("Bool /= " ++ show t))
unify  TInt         TInt           =  ([], TInt)
unify  t            TInt           =  ([], TError ("Int /= " ++ show t))
unify  TInt         t              =  ([], TError ("Int /= " ++ show t))
unify  TUnit        TUnit          =  ([], TUnit)
unify  t            TUnit          =  ([], TError ("() /= " ++ show t))
unify  TUnit        t              =  ([], TError ("() /= " ++ show t))
unify  (TFunc a b)  (TFunc a' b')  =  let  (aSubst,t1)  = unify a a'
                                           (bSubst,t2)  = unify (aSubst  |=>  b) (aSubst  |=>  b')
                                      in (bSubst  |||  aSubst, TFunc t1 t2)
unify  (TFunc a b)  t              =  ([], TError (show (TFunc a b) ++ " /= " ++ show t))
unify  t            (TFunc a b)    =  ([], TError (show (TFunc a b) ++ " /= " ++ show t))
unify  t1           t2             =  sysError ("Invalid types " ++ show t1  
                                        ++ " and " ++ show t2 ++ " in unification.")

unifyMsg t1 t2 =  let  (subst, ttp)  = unify t1 t2
                       (tp,msg)      = getErrorMsg ttp
                  in (subst, tp, msg)

varBind :: VarName -> Type -> (Subst, Type)
varBind v t = if v `occurs` t
                then ([], TError "Occurence check failed")
                else ([(v,t)],t)

occurs v t = case t of
  TVar v'      ->  v == v'
  TBound _     ->  sysError "Occurence check on universally quantified variable."
  TForall i t  ->  occurs v t
  TAny         ->  False
  TInt         ->  False
  TBool        ->  False
  TUnit        ->  False
  TFunc a b    ->  occurs v a || occurs v b


unifyPossiblyAnnoted  (TAny)  t  = (emptysubst,t,empty)
unifyPossiblyAnnoted  t       t' = unifyMsg t t'

sysError s = error ("System error:" ++ s) 
