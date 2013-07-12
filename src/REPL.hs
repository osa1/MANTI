{-# OPTIONS_GHC -Wall #-}
module REPL where

import           DFS
import           Parser
import           Print
import           Types

import           Text.Parsec         (many, parse)

import           Control.Monad.Error
import           Control.Monad.State
import           System.Cmd          (system)
import           System.Directory    (getTemporaryDirectory, removeFile)
import           System.IO


runFile :: FilePath -> Manti ()
runFile path = do
    contents <- liftIO $ readFile path
    loadFileFromString contents

repl :: Manti ()
repl = do
    liftIO $ putStr "?- "
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case parse toplevel "repl" input of
      Left parseError -> liftIO (print parseError) >> repl
      Right (TRule rule') -> runRule rule' >> repl
      Right (TQuery query') -> do
        let qvars = vars query'
        r <- solve [query']
        liftIO . putStrLn $ printResults r qvars
        repl
      Right (TCmd Edit) -> edit >> repl
      Right (TCmd (Load filePath)) -> runFile filePath >> repl

runRule :: Rule -> Manti ()
runRule r = addRule (generalize r)

edit :: Manti ()
edit = do
    tempDir <- liftIO getTemporaryDirectory
    (path, handle) <- liftIO $ openTempFile tempDir "temp.manti"
    rls <- liftM reverse $ gets rules
    liftIO $ do
      hPutStr handle (pprint rls)
      hFlush handle
    liftIO $ do
      void $ system $ "vim " ++ path
      hSeek handle AbsoluteSeek 0
    put defaultMantiState
    loadFileFromHandle handle
    liftIO $ do
      hClose handle
      removeFile path

loadFileFromHandle :: Handle -> Manti ()
loadFileFromHandle handle = do
    contents <- liftIO $ hGetContents handle
    liftIO $ putStrLn contents
    loadFileFromString contents

loadFileFromString :: String -> Manti ()
loadFileFromString s =
    case parse (many queryOrRule) "string" s of
      Left parseError -> liftIO $ print parseError
      Right stats ->
        forM_ stats $ \stat ->
          case stat of
            Right rule' -> runRule rule'
            Left query' -> do
              let qvars = vars query'
              r <- solve [query']
              liftIO . putStrLn $ printResults r qvars

manti :: Manti a -> IO (Either MantiError a)
manti m = evalStateT (runVarGenT (runErrorT (evalStateT (runManti m) defaultMantiState))) 0
