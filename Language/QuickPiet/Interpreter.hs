module Language.QuickPiet.Interpreter where

import Data.List
import Language.QuickPiet.StackOperations
import System.IO

data Interpreter = Interpreter [Command] [Command] Handle Handle Stack
                 | Finished [Command] [Command] Stack

instance Show Interpreter where
    show interpreter = intercalate "\n" [(status interpreter), (lineinfo interpreter), (stackstatus interpreter)]
        where status (Interpreter _ _ _ _ _) = "Executing..."
              status (Finished    _ _ _) = "Finished..."
              lineinfo (Interpreter done rest _ _ _) = lineinfo' done rest
              lineinfo (Finished done rest _) = lineinfo' done rest
              lineinfo' ds [] = "<end of script>"
              lineinfo' ds (c:_) = (show (length ds)) ++ ": " ++ (show c)
              stackstatus (Interpreter _ _ _ _ stack) = show stack
              stackstatus (Finished    _ _ stack) = show stack

execute :: Interpreter -> IO Interpreter
execute finished@(Finished _ _ _) = return finished
execute (Interpreter done []                          inH outH stack)     = return $ Finished done [] stack
execute (Interpreter done (p@(Push x):rest)           inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (push x stack)
execute (Interpreter done (p@Pop:rest)                inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (pop stack)
execute (Interpreter done (p@Duplicate:rest)          inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (duplicate stack)
execute (Interpreter done (p@Roll:rest)               inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (roll stack)
execute (Interpreter done (p@In:rest)                 inH outH stack)     = do stack' <- inop inH stack
                                                                               return $ Interpreter (done ++ [p]) rest inH outH stack'
execute (Interpreter done (p@Out:rest)                inH outH stack)     = do stack' <- outop outH stack
                                                                               return $ Interpreter (done ++ [p]) rest inH outH stack'
execute (Interpreter done (p@Add:rest)                inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (add stack)
execute (Interpreter done (p@Subtract:rest)           inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (subtractop stack)
execute (Interpreter done (p@Multiply:rest)           inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (multiply stack)
execute (Interpreter done (p@Divide:rest)             inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (divide stack)
execute (Interpreter done (p@Mod:rest)                inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (modop stack)
execute (Interpreter done (p@Not:rest)                inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (notop stack)
execute (Interpreter done (p@Greater:rest)            inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  (greater stack)
execute (Interpreter done (p@End:rest)                inH outH stack)     = return $ Finished    (done ++ [p]) rest  stack
execute (Interpreter done (p@(Label _):rest)          inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  stack
execute (Interpreter done (p@(Comment _):rest)        inH outH stack)     = return $ Interpreter (done ++ [p]) rest  inH  outH  stack
execute (Interpreter done (p@Blank:rest)              inH outH stack)     = execute (Interpreter (done ++ [p]) rest inH outH stack)
execute (Interpreter done (p@(Goto label other):rest) inH outH (x:stack)) = return $ Interpreter done'         rest' inH  outH  stack
    where (done', rest') = goto x (done ++ [p] ++ rest)
          goto 1 prog = break (== (Label label)) prog
          goto 3 prog = break (== (Label other)) prog
          goto _ prog = (done ++ [p], rest)

initialize :: [Command] -> Handle -> Handle -> Interpreter
initialize script inH outH = Interpreter [] script inH outH []

-- takes the list of commands to execute and stdin, returns stdout
complete :: Interpreter -> IO Interpreter
complete finished@(Finished _ _ _) = return $ finished
complete interpreter = do interpreter' <- (execute interpreter)
                          complete interpreter'