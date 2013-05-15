{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Unify where

import Types

import qualified Data.Map as M

import Control.Monad.Error
import Control.Monad.Identity

type Substs    = M.Map String Term
type Error e a = ErrorT e Identity a

nullSubst :: Substs
nullSubst = M.empty

varBind :: Substs -> String -> Term -> Substs
varBind substs var term =
    let substs' = M.mapWithKey (\key term' -> if key == var then term else term') substs
     in M.insert var term substs'

functorName :: String -> Int -> String
functorName s i = s ++ "/" ++ show i

unify' :: Substs -> Term -> Term -> ErrorT MantiError Identity Substs 
unify' substs a1@(Atom atom1) a2@(Atom atom2)
  | atom1 == atom2 = return substs
  | otherwise      = throwError $ UnificationError a1 a2
unify' substs (Var var) term = return $ varBind substs var term
unify' substs term (Var var) = return $ varBind substs var term
unify' substs (Compound fName1 terms1) (Compound fName2 terms2)
  | length terms1 /= length terms2 =
      let functor1 = functorName fName1 (length terms1)
          functor2 = functorName fName2 (length terms2)
       in throwError $ UnificationError (Var functor1) (Var functor2)
  | otherwise = do
      substs' <- unify' substs (Atom fName1) (Atom fName2)
      foldM unify'' substs' (zip terms1 terms2)
      where
        unify'' :: Substs -> (Term, Term) -> ErrorT MantiError Identity Substs
        unify'' substs (t1, t2) = unify' substs t1 t2
unify' _ VGen{} _ = error "VGen in unify."
unify' _ _ VGen{} = error "VGen in unify."
unify' _ t1 t2 = throwError $ UnificationError t1 t2

unifyWSubsts :: MonadError MantiError m => Substs -> Term -> Term -> m Substs
unifyWSubsts substs t1 t2 = either throwError return $ runIdentity (runErrorT (unify' substs t1 t2))
