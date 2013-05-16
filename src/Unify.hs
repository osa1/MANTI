{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Unify
  ( mgu
  , nullSubst
  , apply
  , Substs
  ) where

import           Types

import qualified Data.Map                   as M
import           Control.Monad.Error
import           Control.Monad.Identity

type Substs = M.Map Var Term

nullSubst :: Substs
nullSubst = M.empty

class Apply a where
    apply :: Substs -> a -> a

instance Apply Term where
    apply _ a@TAtom{} = a
    apply substs (TVar var) =
        case M.lookup var substs of
          Nothing -> TVar var
          Just t' -> t'
    apply substs (TComp (Compound fname terms)) = TComp (Compound fname (map (apply substs) terms))
    apply _ TVGen{} = error "VGen in apply"

instance Apply Compound where
    apply substs (Compound fName args) = Compound fName (apply substs args)

instance Apply a => Apply [a] where
    apply substs l = map (apply substs) l

varBind :: Substs -> Var -> Term -> Substs
varBind substs var term =
    let substs' = M.mapWithKey (\key term' -> if key == var then term else term') substs
     in M.insert var term substs'

functorName :: Atom -> Int -> String
functorName (Atom s) i = s ++ "/" ++ show i

unify' :: Substs -> Term -> Term -> ErrorT MantiError Identity Substs
unify' substs a1@(TAtom atom1) a2@(TAtom atom2)
  | atom1 == atom2 = return substs
  | otherwise      = throwError $ UnificationError a1 a2
unify' substs (TVar var) term = return $ varBind substs var term
unify' substs term (TVar var) = return $ varBind substs var term
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
        unify_fold ss (t1, t2) = unify' ss (apply ss t1) (apply ss t2)
unify' _ TVGen{} _ = error "VGen in unify."
unify' _ _ TVGen{} = error "VGen in unify."
unify' _ t1 t2 = throwError $ UnificationError t1 t2

unify :: Substs -> Term -> Term -> Either MantiError Substs
unify substs t1 t2 = runIdentity (runErrorT (unify' substs t1 t2))

mgu :: Substs -> Term -> Term -> Either MantiError Substs
mgu substs t1 t2 = unify substs (apply substs t1) (apply substs t2)
