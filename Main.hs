-- file: QuickPiet.hs
-- An implementation of QuickPiet by Ben Lee
-- based on the Piet language by ???

module Main where

import Control.Monad.State
import Language.QuickPiet.Parser
import Language.QuickPiet.StackOperations
import System
import System.IO

-- the main will take a filename, open it, parse it and pass the parsed commands to the interpreter
-- run the interpreter with the input and output hooked up to stdout and stdin should be simple :)
main = do putStr "> "
          hFlush stdout
          input <- getLine
          case parseAction input of
            Left error -> putStrLn $ "Error: " ++ (show error)
            Right command -> let stack = execState command [] in
                             do putStrLn $ showStack stack