{-# OPTIONS_GHC -Wall #-}
module REPL where

import Types
import Parser

import Text.Parsec (parse)
import Control.Monad.State
import Control.Monad.Error
import System.IO

mantiRepl :: Manti ()
mantiRepl = do
    liftIO $ putStr "?- "
    liftIO $ hFlush stdout
    input <- liftIO $ getLine
    case parse stat "repl" input of
      Left parseError -> liftIO (print parseError) >> mantiRepl
      Right (Rule rhead rbody) -> do
        let gs@(Rule rhead' rbody') = generalize (Rule rhead rbody)
        liftIO $ putStrLn $ "generalized to: " ++ show gs
        addRule (rhead', rbody')
        mantiRepl
      Right Query{} -> liftIO (putStrLn "queries are not supported yet.") >> mantiRepl

manti :: Manti a -> IO (Either MantiError a)
manti m = evalStateT (runVarGenT (runErrorT (evalStateT (runManti m) (MantiState{rules = []})))) 0

main :: IO ()
main = do
    r <- manti (mantiRepl)
    print r
