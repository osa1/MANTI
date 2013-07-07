{-# OPTIONS_GHC -Wall #-}
module Main where

import           REPL

import           System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    r <- if not (null args)
           then manti $ runFile (head args)
           else manti repl
    print r

