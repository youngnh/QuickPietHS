-- file: QuickPiet.hs
-- An implementation of QuickPiet by Ben Lee
-- based on the Piet language by ???

module Main where

import Language.QuickPiet.Parser
import Language.QuickPiet.Interpreter
import System
import System.IO

-- the main will take a filename, open it, parse it and pass the parsed commands to the interpreter
-- run the interpreter with the input and output hooked up to stdout and stdin should be simple :)
main = do (path:args) <- getArgs
          contents <- readFile path
          instr <- getContents
          case parseScript contents of
            Left err -> putStrLn $ "uh oh, error: " ++ show err
            Right script -> let (Finished _ _ _ outstr _) = complete (initialize script instr) in
                            putStr outstr
