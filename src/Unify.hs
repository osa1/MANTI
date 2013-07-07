{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Unify
  ( nullSubst
  , varSubsts
  , unify
  , apply
  , Substs
  , unionSubsts
  , unionSubsts'
  ) where

import           Types

import qualified Data.Map                   as M
import           Control.Monad.Error
import           Control.Monad.Identity
import qualified Data.Set as S

type Substs = M.Map Var Term

nullSubst :: Substs
nullSubst = M.empty

varSubsts :: S.Set Var -> Substs -> Substs
varSubsts varSet substs = S.fold varS nullSubst varSet
  where
    varS :: Var -> Substs -> Substs
    varS var ss = M.insert var (lastSubst var substs) ss

    lastSubst :: Var -> Substs -> Term
    lastSubst var ss =
      case M.lookup var ss of
        Nothing          -> TVar var
        Just (TVar var') -> lastSubst var' ss
        Just term        -> term

class Apply a where
    apply :: Substs -> a -> a

instance Apply Term where
    apply _ a@TAtom{} = a
    apply substs (TVar var) =
        case M.lookup var substs of
          Nothing -> TVar var
          Just t' -> apply substs t'
    apply substs (TComp (Compound fname terms)) = TComp (Compound fname (map (apply substs) terms))
    apply _ TVGen{} = error "VGen in apply"

instance Apply Compound where
    apply substs (Compound fName args) = Compound fName (apply substs args)

instance Apply Query where
    apply substs (Query c) = Query (apply substs c)

instance Apply a => Apply [a] where
    apply substs = map (apply substs)

occursCheck :: Var -> Term -> Bool
occursCheck _ TAtom{} = False
occursCheck (Var v) (TVar (Var v'))
  | v == v'   = True
  | otherwise = False
occursCheck v (TComp (Compound _ terms)) =
    any (occursCheck v) terms
occursCheck _ TVGen{} = False

varBind :: Var -> Term -> Substs -> Either MantiError Substs
varBind v t ss
  | occursCheck v t = throwError OccursCheck
  | otherwise       = Right $ M.insert v t ss

unionSubsts :: Substs -> Substs -> Either MantiError Substs
unionSubsts s1 s2 = foldM (flip $ uncurry varBind) s1 (M.toList s2)

unionSubsts' :: Substs -> Substs -> Manti Substs
unionSubsts' s1 s2 = either throwError return $ unionSubsts s1 s2

functorName :: Atom -> Int -> String
functorName (Atom s) i = s ++ "/" ++ show i

unify' :: Substs -> Term -> Term -> ErrorT MantiError Identity Substs
unify' substs a1@(TAtom atom1) a2@(TAtom atom2)
  | atom1 == atom2 = return substs
  | otherwise      = throwError $ UnificationError a1 a2
unify' substs (TVar var) term = either throwError return $ varBind var term substs
unify' substs term (TVar var) = either throwError return $ varBind var term substs
unify' substs (TComp (Compound fName1 terms1)) (TComp (Compound fName2 terms2))
  | length terms1 /= length terms2 =
      let functor1 = functorName fName1 (length terms1)
          functor2 = functorName fName2 (length terms2)
       in throwError $ UnificationError (TVar $ Var functor1) (TVar $ Var functor2)
  | otherwise = do
      substs' <- unify' substs (TAtom fName1) (TAtom fName2)
      foldM unify_fold substs' (zip terms1 terms2)
      where
        unify_fold :: Substs -> (Term, Term) -> ErrorT MantiError Identity Substs
        unify_fold ss (t1, t2) = unify' ss t1 t2
unify' _ TVGen{} _ = error "VGen in unify."
unify' _ _ TVGen{} = error "VGen in unify."
unify' _ t1 t2 = throwError $ UnificationError t1 t2

unify :: Substs -> Term -> Term -> Either MantiError Substs
unify substs t1 t2 = runIdentity (runErrorT (unify' substs t1 t2))
