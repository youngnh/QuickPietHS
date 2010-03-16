module Language.QuickPiet.Interpreter 
    (Interpreter(..)
    ,execute
    ,nstep
    ,initialize
    ,complete
    )where

import Data.List
import Language.QuickPiet.StackOperations

data Interpreter = Interpreter [Command] [Command] String String Stack
                 | Finished [Command] [Command] String String Stack

instance Show Interpreter where
    show interpreter = intercalate "\n" [(status interpreter), (lineinfo interpreter), (stackstatus interpreter)]
        where status (Interpreter _ _ _ _ _) = "Executing..."
              status (Finished    _ _ _ _ _) = "Finished..."
              lineinfo (Interpreter done rest _ _ _) = lineinfo' done rest
              lineinfo (Finished done rest _ _ _) = lineinfo' done rest
              lineinfo' ds [] = "<end of script>"
              lineinfo' ds (c:_) = (show (length ds)) ++ ": " ++ (show c)
              stackstatus (Interpreter _ _ _ _ stack) = show stack
              stackstatus (Finished    _ _ _ _ stack) = show stack

execute :: Interpreter -> Interpreter
execute finished@(Finished _ _ _ _ _) = finished
execute (Interpreter done []                          instr outstr stack)     = Finished done [] instr outstr stack
execute (Interpreter done (p@(Push x):rest)           instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (push x stack)
execute (Interpreter done (p@Pop:rest)                instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (pop stack)
execute (Interpreter done (p@Duplicate:rest)          instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (duplicate stack)
execute (Interpreter done (p@Roll:rest)               instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (roll stack)
execute (Interpreter done (p@In:rest)                 instr outstr stack)     = Interpreter (done ++ [p]) rest  instr' outstr  stack'
    where (instr', stack') = inop instr stack
execute (Interpreter done (p@Out:rest)                instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr' stack'
    where (outstr', stack') = outop outstr stack
execute (Interpreter done (p@Add:rest)                instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (add stack)
execute (Interpreter done (p@Subtract:rest)           instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (subtractop stack)
execute (Interpreter done (p@Multiply:rest)           instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (multiply stack)
execute (Interpreter done (p@Divide:rest)             instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (divide stack)
execute (Interpreter done (p@Mod:rest)                instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (modop stack)
execute (Interpreter done (p@Not:rest)                instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (notop stack)
execute (Interpreter done (p@Greater:rest)            instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  (greater stack)
execute (Interpreter done (p@End:rest)                instr outstr stack)     = Finished    (done ++ [p]) rest  instr  outstr  stack
execute (Interpreter done (p@(Label _):rest)          instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  stack
execute (Interpreter done (p@(Comment _):rest)            instr outstr stack)     = Interpreter (done ++ [p]) rest  instr  outstr  stack
execute (Interpreter done (p@(Goto label other):rest) instr outstr (Stack (x:stack))) = Interpreter done'         rest' instr  outstr  (Stack stack)
    where (done', rest') = goto x (done ++ [p] ++ rest)
          goto 1 prog = break (== (Label label)) prog
          goto 3 prog = break (== (Label other)) prog

nstep :: Int -> Interpreter -> Interpreter
nstep 0 interpreter = interpreter
nstep n interpreter = nstep (n - 1) (execute interpreter)

initialize :: [Command] -> String -> Interpreter
initialize script instr = Interpreter [] script instr "" (Stack [])

-- takes the list of commands to execute and stdin, returns stdout
complete :: Interpreter -> Interpreter
complete finished@(Finished _ _ _ _ _) = finished
complete interpreter = complete (execute interpreter)
