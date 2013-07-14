{-# OPTIONS_GHC -Wall #-}
module REPL where

import           DFS
import           Parser
import           Print
import           Types

import           Control.Monad.Error
import           Control.Monad.State
import           System.Cmd          (system)
import           System.Directory    (getTemporaryDirectory, removeFile)
import           System.IO
import           Text.Parsec         (many)


repl :: Manti ()
repl = do
    liftIO $ putStr "?- "
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case parse toplevel "repl" input of
      Left parseError       -> liftIO (print parseError) >> repl
      Right (TRule rule')   -> runRule rule' >> repl
      Right (TQuery query') -> runQuery query' >> repl
      Right (TCmd cmd)      -> runCmd cmd >> repl

runRule :: Rule -> Manti ()
runRule r = addRule (generalize r)

runCmd :: Cmd -> Manti ()
runCmd Edit = edit
runCmd (Load files) = do
    put defaultMantiState
    mapM_ runFile files

runQuery :: Query -> Manti ()
runQuery query' = do
    let qvars = vars query'
    r <- solve [query']
    liftIO . putStrLn $ printResults r qvars

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

runFile :: FilePath -> Manti ()
runFile path = do
    modules <- gets loadedModules
    unless (path `elem` modules) $ do
      liftIO $ putStrLn $ "loading file: " ++ show path
      contents <- liftIO $ readFile path
      loadFileFromString contents
      modify (addModule path)

loadFileFromHandle :: Handle -> Manti ()
loadFileFromHandle handle = do
    contents <- liftIO $ hGetContents handle
    liftIO $ putStrLn contents
    loadFileFromString contents

loadFileFromString :: String -> Manti ()
loadFileFromString s =
    case parse (many toplevel) "string" s of
      Left parseError -> liftIO $ print parseError
      Right stats -> do
        forM_ stats $ \stat ->
          case stat of
            TRule rule'       -> runRule rule'
            TQuery query'     -> runQuery query'
            TCmd Edit         -> liftIO $ putStrLn "Warning: Edit command in files are ignored."
            TCmd (Load files) -> runCmd (Load files)
        liftIO $ putStrLn s

manti :: Manti a -> IO (Either MantiError a)
manti m = evalStateT (runVarGenT (runErrorT (evalStateT (runManti m) defaultMantiState))) 0
