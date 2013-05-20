{-# OPTIONS_GHC -Wall #-}
module REPL where

import Types
import Parser

import Text.Parsec (parse, many)

import System.Environment (getArgs)
import Control.Monad.State
import Control.Monad.Error
import System.IO

import DFS

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
runRule r = do
    let r' = generalize r
    addRule r'

manti :: Manti a -> IO (Either MantiError a)
manti m = evalStateT (runVarGenT (runErrorT (evalStateT (runManti m) defaultMantiState))) 0

main :: IO ()
main = do
    args <- getArgs
    if not (null args)
      then runFile (head args)
      else do
        r <- manti mantiRepl
        print r
