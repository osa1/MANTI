{-# OPTIONS_GHC -Wall #-}
module REPL where

import Types
import Parser
import DFS
import Print

import Text.Parsec (parse, many)

import System.Environment (getArgs)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Cmd (system)
import System.IO
import Control.Monad.State
import Control.Monad.Error


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
        r <- solve [query']
        liftIO $ print r
        repl
      Right (TCmd Edit) -> edit >> repl

runRule :: Rule -> Manti ()
runRule r = do
    let r' = generalize r
    addRule r'

edit :: Manti ()
edit = do
    tempDir <- liftIO $ getTemporaryDirectory
    (path, handle) <- liftIO $ openTempFile tempDir "temp.manti"
    rls <- liftM reverse $ gets rules
    liftIO $ do
      hPutStr handle (pprint rls)
      hFlush handle
    void $ liftIO $ system $ "vim " ++ path
    loadFileFromHandle handle
    liftIO $ do
      hClose handle
      removeFile path

loadFileFromHandle :: Handle -> Manti ()
loadFileFromHandle handle = do
    contents <- liftIO $ hGetContents handle
    loadFileFromString contents

loadFileFromString :: String -> Manti ()
loadFileFromString s = do
    put defaultMantiState
    case parse (many queryOrRule) "string" s of
      Left parseError -> liftIO $ print parseError
      Right stats -> do
        forM_ stats $ \stat ->
          case stat of
            Right rule' -> runRule rule'
            Left query' -> do
              r <- solve [query']
              liftIO $ print r

manti :: Manti a -> IO (Either MantiError a)
manti m = evalStateT (runVarGenT (runErrorT (evalStateT (runManti m) defaultMantiState))) 0

main :: IO ()
main = do
    args <- getArgs
    r <- if not (null args)
           then manti $ runFile (head args)
           else manti repl
    print r
