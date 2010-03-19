-- file: QuickPiet.hs
-- An implementation of QuickPiet by Ben Lee
-- based on the Piet language by ???

module Main where

import Control.Monad.State
import Language.QuickPiet.Parser
import Language.QuickPiet.StackOperations
import System
import System.IO

prompt :: IO String
prompt = do
  putStr "> "
  hFlush stdout
  getLine

exec :: String -> Stack -> IO Stack
exec input s =
    case parseAction input of
      Left error -> do 
        putStrLn $ "Error: " ++ (show error)
        return s
      Right command -> let stack = execState command s in
                       do putStrLn $ showStack stack
                          hFlush stdout
                          return stack

interpLoop stack = do
  input <- prompt
  stack' <- exec input stack
  interpLoop stack'

main :: IO ()
main = interpLoop []