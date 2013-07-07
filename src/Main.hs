{-# OPTIONS_GHC -Wall #-}
module Main where

import           REPL

import           System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    r <- manti $ if not (null args)
                   then runFile (head args)
                   else repl
    print r
