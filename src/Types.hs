{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad.State
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.Map as M

data Stat
    = Rule RHead RBody
    | Query Term
    deriving Show

data RHead = RHead String [Term] deriving Show
data RBody = RBody [Term]        deriving Show

data Term
    = Atom String
    | Var String
    | Compound String [Term]
    | VGen Int
    deriving Show

newtype VarGenT m a = VarGen { runVarGen :: StateT Int m a }
    deriving (Functor, Applicative, Monad, MonadState Int, MonadTrans, MonadIO)

type VarGen = VarGenT Identity

data MantiState = MantiState { rules :: [ (RHead, RBody) ] }
    deriving Show

newtype Manti a = Manti { runManti :: StateT MantiState (ErrorT MantiError (VarGenT IO)) a }
    deriving (Functor, Applicative, Monad, MonadState MantiState, MonadError MantiError, MonadIO)

data MantiError = ErrMsg String
    deriving Show

instance Error MantiError where
    strMsg = ErrMsg

addRule :: (RHead, RBody) -> Manti ()
addRule rule = modify (\s -> s { rules = rule:rules s })

freshVar :: VarGen Int
freshVar = do
    lastVar <- get
    modify (+ 1)
    return lastVar

instantiate :: Stat -> VarGen Stat
instantiate = flip evalStateT M.empty . iter
  where
    iter :: Stat -> StateT (M.Map Int String) VarGen Stat
    iter Query{} = error "can't instantiate query"
    iter (Rule (RHead predName args) (RBody conjs)) = do
      arggen <- mapM iterTerm args
      conjsgen <- mapM iterTerm conjs
      return $ Rule (RHead predName arggen) (RBody conjsgen)

    iterTerm :: Term -> StateT (M.Map Int String) VarGen Term
    iterTerm (Atom s) = return (Atom s)
    iterTerm (Var s) = return (Var s)
    iterTerm (Compound fName args) = Compound fName <$> mapM iterTerm args
    iterTerm (VGen i) = do
      freshs <- get
      case M.lookup i freshs of
        Just s -> return $ Var s
        Nothing -> do
          fresh <- lift freshVar
          modify (M.insert i ("_V" ++ show fresh))
          return (Var $ "_V" ++ show fresh)

generalize :: Stat -> Stat
generalize = flip evalState M.empty . iter
  where
    iter :: Stat -> State (M.Map String Int) Stat
    iter Query{} = error "can't generalize query"
    iter (Rule (RHead predName args) (RBody conjs)) = do
      arggen <- mapM iterTerm args
      conjsgen <- mapM iterTerm conjs
      return $ Rule (RHead predName arggen) (RBody conjsgen)

    iterTerm :: Term -> State (M.Map String Int) Term
    iterTerm (Atom s) = return (Atom s)
    iterTerm (Var s) = do
      gens <- get
      case M.lookup s gens of
        Just i -> return $ VGen i
        Nothing -> do
          let size = M.size gens
          put (M.insert s size gens)
          return $ VGen size
    iterTerm (Compound functor terms) = Compound functor <$> mapM iterTerm terms
    iterTerm VGen{} = error "VGen in iterTerm"
