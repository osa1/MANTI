{-# OPTIONS_GHC -Wall #-}
module REPL where

import Types
import Parser
import Unify

import Text.Parsec (parse, (<|>), try, many)
import Text.Parsec.String (Parser)

import System.Environment (getArgs)
import Data.Maybe (isJust)
import Control.Monad.State
import Control.Monad.Error
import System.IO

import DFS

queryOrRule :: Parser (Either Query Rule)
queryOrRule = liftM Left (try query) <|> liftM Right rule

runFile :: FilePath -> IO ()
runFile path = do
    contents <- readFile path
    case parse (many queryOrRule) path contents of
      Left parseError -> print parseError
      Right stats -> do
        r <- manti $ forM_ stats $ \stat ->
          case stat of
            Right rule' -> runRule rule'
            Left query' -> do
              r <- solve [query']
              liftIO $ print r
        print r

mantiRepl :: Manti ()
mantiRepl = do
    liftIO $ putStr "?- "
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case parse queryOrRule "repl" input of
      Left parseError -> liftIO (print parseError) >> mantiRepl
      Right (Right rule') -> runRule rule' >> mantiRepl
      Right (Left query') -> do
        r <- solve [query']
        liftIO $ print r
        mantiRepl

runRule :: Rule -> Manti ()
runRule rule = do
    let rule' = generalize rule
    addRule rule'

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
    args <- getArgs
    if not (null args)
      then runFile (head args)
      else do
        r <- manti mantiRepl
        print r
