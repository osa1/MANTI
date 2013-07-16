{-# OPTIONS_GHC -Wall #-}
module DFS where

import           Types
import           Unify

import           Control.Monad       (forM, liftM)
import           Control.Monad.Error (strMsg, throwError)
import           Control.Monad.State (gets)
import           Data.Maybe          (catMaybes)

--import Debug.Trace

getMatchingRules :: Compound -> Manti [(Rule, Substs)]
--getMatchingRules c | trace ("getMatchingRule " ++ show c) False = undefined
getMatchingRules (Compound fName args) = do
    rls <- gets rules
    let rls' = lookupRules fName (length args) rls
    rinsts <- mapM instantiate rls'
    return $ catMaybes $ flip map rinsts $ \r@(Rule (RHead _ rargs) _) ->
      case unifyArgs nullSubst args rargs of
        Nothing -> Nothing
        Just ss -> Just (r, ss)
  where
    lookupRules :: Atom -> Int -> [Rule] -> [Rule]
    lookupRules name arity =
        filter (\(Rule (RHead fname rargs) _) -> name == fname && length rargs == arity)

    unifyArgs :: Substs -> [Term] -> [Term] -> Maybe Substs
    unifyArgs ss [] [] = Just ss
    unifyArgs ss (t1:t1r) (t2:t2r) =
        case unify ss (apply ss t1) (apply ss t2) of
          Left _ -> Nothing
          Right ss' -> unifyArgs ss' t1r t2r

solve :: [Query] -> Manti [Substs]
solve [] = return [nullSubst]
solve (Query (Compound (Atom "not") [arg]):gs) =
    case arg of
      TComp comp -> do
        ss <- solve [Query comp]
        --trace ("ss in `not': " ++ show ss) (return ())
        if null ss then solve gs else return []
      term -> throwError . strMsg $ "not arg is not compound: " ++ show term
solve goals = do
    bs <- branch goals
    --trace ("bs: " ++ show bs) (return ())
    liftM concat $ forM bs $ \(s, goals') -> do
      solutions <- solve goals'
      mapM (unionSubsts' s) solutions

branch :: [Query] -> Manti [(Substs, [Query])]
branch [] = return []
branch (Query c:rest) = do
    rls <- getMatchingRules c
    --trace ("matching rules: " ++ show rls) (return ())
    --trace ("rest: " ++ show rest) (return ())
    return $ flip map rls $ \(Rule _ (RBody conjs), ss) -> (ss, apply ss $ map Query conjs ++ rest)
