{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Set            as S


data Rule  = Rule RHead RBody  deriving Show
data RHead = RHead Atom [Term] deriving Show
data RBody = RBody [Compound]  deriving Show

data Query = Query Compound deriving Show

newtype Atom = Atom String deriving (Show, Eq, Ord)
newtype Var  = Var  String deriving (Show, Eq, Ord)
data Compound = Compound Atom [Term] deriving (Show, Eq)

-- how to implement foldable instance for Term?
-- it's mutually recursive structure(with compound) makes it harder
data Term
    = TAtom Atom
    | TVar Var
    | TComp Compound
    | TInt Integer
    | TVGen Int
    deriving (Show, Eq)

class HasVar a where
    vars :: a -> S.Set Var

instance HasVar Term where
    vars = iter S.empty
      where
        iter :: S.Set Var -> Term -> S.Set Var
        iter acc TAtom{}    = acc
        iter acc (TVar var) = S.insert var acc
        iter acc (TComp c)  = acc `S.union` vars c
        iter acc TInt{}     = acc
        iter acc TVGen{}    = acc

instance HasVar Compound where
    vars (Compound _ terms) = S.unions $ map vars terms

instance HasVar Query where
    vars (Query c) = vars c

newtype VarGenT m a = VarGenT { runVarGenT :: StateT Int m a }
    deriving (Functor, Applicative, Monad, MonadState Int, MonadTrans, MonadIO)

data MantiState = MantiState { rules :: [ Rule ], lastVar :: Int }
    deriving Show

defaultMantiState :: MantiState
defaultMantiState = MantiState{ rules = [oneTrueRule], lastVar = 0 }
  where
    oneTrueRule :: Rule
    oneTrueRule = Rule (RHead (Atom "true") []) (RBody [])

newtype Manti a = Manti { runManti :: StateT MantiState (ErrorT MantiError (VarGenT IO)) a }
    deriving (Functor, Applicative, Monad, MonadState MantiState, MonadError MantiError, MonadIO)

data MantiError
    = ErrMsg String
    | UnificationError Term Term
    | UndefinedRule Atom Int
    | OccursCheck
    deriving (Show, Eq)

instance Error MantiError where
    strMsg = ErrMsg

addRule :: Rule -> Manti ()
addRule rule = modify (\s -> s{ rules = rule:rules s })

freshVar :: Manti Int
freshVar = do
    v <- gets lastVar
    modify (\s -> s{lastVar = v+1})
    return v

instantiate :: Rule -> Manti Rule
instantiate = flip evalStateT M.empty . iter
  where
    iter :: Rule -> StateT (M.Map Int String) Manti Rule
    iter (Rule (RHead predName args) (RBody conjs)) = do
      arggen <- mapM iterTerm args
      conjsgen <- mapM iterComp conjs
      return $ Rule (RHead predName arggen) (RBody conjsgen)

    iterTerm :: Term -> StateT (M.Map Int String) Manti Term
    iterTerm r@TAtom{} = return r
    iterTerm r@TVar{}  = return r
    iterTerm (TComp (Compound fName args)) =
      liftM (TComp . Compound fName) $ mapM iterTerm args
    iterTerm r@TInt{}  = return r
    iterTerm (TVGen i) = do
      freshs <- get
      case M.lookup i freshs of
        Just s -> return $ TVar (Var s)
        Nothing -> do
          fresh <- lift freshVar
          modify (M.insert i ("_V" ++ show fresh))
          return $ TVar (Var $ "_V" ++ show fresh)

    iterComp :: Compound -> StateT (M.Map Int String) Manti Compound
    iterComp (Compound fName args) =
      liftM (Compound fName) $ mapM iterTerm args

generalize :: Rule -> Rule
generalize = flip evalState M.empty . iter
  where
    iter :: Rule -> State (M.Map String Int) Rule
    iter (Rule (RHead predName args) (RBody conjs)) = do
      arggen <- mapM iterTerm args
      conjsgen <- mapM iterComp conjs
      return $ Rule (RHead predName arggen) (RBody conjsgen)

    iterTerm :: Term -> State (M.Map String Int) Term
    iterTerm r@TAtom{} = return r
    iterTerm (TVar (Var s)) = do
      gens <- get
      case M.lookup s gens of
        Just i -> return $ TVGen i
        Nothing -> do
          let size = M.size gens
          put (M.insert s size gens)
          return $ TVGen size
    iterTerm (TComp (Compound fName args)) =
      liftM (TComp . Compound fName) $ mapM iterTerm args
    iterTerm r@TInt{} = return r
    iterTerm TVGen{} = error "TVGen in iterTerm"

    iterComp :: Compound -> State (M.Map String Int) Compound
    iterComp (Compound fName args) =
      liftM (Compound fName) $ mapM iterTerm args
