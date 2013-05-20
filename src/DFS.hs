{-# OPTIONS_GHC -Wall #-}
module DFS where

import Types
import Unify

import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Error (throwError)
import Control.Monad.State (gets)
import Control.Monad.IO.Class (liftIO)

import Debug.Trace

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
        filter (\(Rule (RHead fname args) _) -> name == fname && length args == arity)

    unifyArgs :: Substs -> [Term] -> [Term] -> Maybe Substs
    unifyArgs ss [] [] = Just ss
    unifyArgs ss (t1:t1r) (t2:t2r) =
        case unify ss (apply ss t1) (apply ss t2) of
          Left _ -> Nothing
          Right ss' -> unifyArgs ss' t1r t2r

concatNonEmpty :: [[Substs]] -> [Substs]
concatNonEmpty r = foldr ((++) . filter (not . M.null)) [] r

solve :: [Query] -> Manti [Substs]
solve [] = return [nullSubst]
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
