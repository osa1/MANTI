{-# OPTIONS_GHC -Wall #-}
module REPL where

import Types
import Parser
import Unify

import Text.Parsec (parse, (<|>), try)
import Text.Parsec.String (Parser)

import Data.Maybe (isJust)
import Control.Monad.State
import Control.Monad.Error
import System.IO

queryOrRule :: Parser (Either Query Rule)
queryOrRule = liftM Left (try query) <|> liftM Right rule

mantiRepl :: Manti ()
mantiRepl = do
    liftIO $ putStr "?- "
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case parse queryOrRule "repl" input of
      Left parseError -> liftIO (print parseError) >> mantiRepl
      Right (Right (Rule rhead rbody)) -> do
        let rule' = generalize (Rule rhead rbody)
        liftIO $ putStrLn $ "generalized to: " ++ show rule'
        addRule rule'
        mantiRepl
      Right (Left query') -> do
        r <- runQuery query'
        liftIO $ print r
        mantiRepl

runQuery :: Query -> Manti (Maybe Substs)
runQuery (Query (Compound fName args)) = do
    rules' <- lookupRules fName (length args)
    when (null rules') $ throwError (UndefinedRule fName (length args))
    ruleinsts <- mapM instantiate rules'
    searchRule nullSubst args ruleinsts

searchRule :: Substs -> [Term] -> [Rule] -> Manti (Maybe Substs)
searchRule ss _ [] = return $ Just ss
searchRule ss terms (Rule (RHead _ args) (RBody conjs):rest) = do
    let substs = unifyArgs ss terms args
    case substs of
      Left _ -> return Nothing
      Right substs' -> do
        r <- liftM (all isJust) $ mapM (runQuery . Query . apply substs') conjs
        searchRule substs' terms rest
  where
    unifyArgs :: Substs -> [Term] -> [Term] -> Either MantiError Substs
    unifyArgs s [] [] = Right s
    unifyArgs s (t1:t1r) (t2:t2r) = do
      s' <- mgu s t1 t2
      unifyArgs s' t1r t2r

lookupRules :: Atom -> Int -> Manti [Rule]
lookupRules name arity = do
    rs <- gets rules
    return $ filter (\(Rule (RHead fname args) _) -> name == fname && length args == arity) rs

manti :: Manti a -> IO (Either MantiError a)
manti m = evalStateT (runVarGenT (runErrorT (evalStateT (runManti m) defaultMantiState))) 0

fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left err) = error $ show err

testTerm, testTerm2 :: Term
testTerm = fromRight (parse term "testTerm" "father(X, X)")
testTerm2 = fromRight (parse term "testTerm2" "father(test, Y)")

main :: IO ()
main = do
    r <- manti mantiRepl
    print r
