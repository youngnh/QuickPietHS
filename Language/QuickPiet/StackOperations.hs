-- These are the stack operations which follow the actions available with the Piet programming languages with some 
-- small changes to allow algorithms to be tested without the need of creating valid Piet images.
-- Original Piet information can be found at http://www.dangermouse.net/esoteric/piet.html
{-# LANGUAGE DeriveDataTypeable #-}
module Language.QuickPiet.StackOperations where

import Control.Exception
import Control.Monad.State
import Data.Char
import Data.Typeable

type Stack = [Int]

data StackException = StackException String
                      deriving (Typeable)

instance Exception StackException
instance Show StackException where
    show (StackException s) = s

showStack :: Stack -> String
showStack []     = "$"
showStack (x:xs) = (show x) ++ " " ++ (showStack xs)

type Command a = State Stack a

-- push X
-- Pushes the value of X onto the stack.  X should be a positive integer
push :: Int -> Command ()
push elt = do stack <- get
              put (elt:stack)

-- pop
-- Pops the top value of the stack and discards
pop :: Command ()
pop = do (elt:stack) <- get
         put stack

-- duplicate
-- Pushes a copy of the top value of the stack onto the stack
duplicate :: Command ()
duplicate = do (x:stack) <- get
               put (x:x:stack)

-- roll
-- Pops the top two values, and "rolls" the remaining stack entries to a depth equal to the second value popped ...
-- By a number of rolls equal to the first value popped ...
-- A single roll to depth n is defined as burying the top value on the stack n deep ...
-- And bringing all values above it up by 1 place ...
-- A negative number of rolls rolls in the opposite direction
roll :: Command ()
roll = do (x:y:stack) <- get
          let (top, bot) = splitAt y stack
          put (roll' x top ++ bot)
    where roll' 0 lst = lst
          roll' n (elt:lst) = roll' (n - 1) (lst ++ [elt])

-- in
-- Read a single value from STDIN and push it onto the stack; characters are read as their ASCII value
inop :: Char -> Command ()
inop c = do stack <- get
            put $ ord c : stack

-- out
-- Pop the top value from the stack and append it's ASCII character value to STDOUT
outop :: Command Char
outop = do (c:stack) <- get
           put stack
           return (chr c)

binaryStackOp :: (Int -> Int -> Int) -> Command ()
binaryStackOp f = do (x:y:stack) <- get
                     put $ (f x y) : stack

-- add
-- Pops the top two values, adds them, and pushes the result
add :: Command ()
add = binaryStackOp (+)

-- subtract
-- Pops the top two values, subtracts the top value from the second top value, and pushes the result
subtractop :: Command ()
subtractop = binaryStackOp (-)

-- multiply
-- Pops the top two values, multiplies them, and pushes the result
multiply :: Command ()
multiply = binaryStackOp (*)

-- divide
-- Pops the top two values, integer divides the second top value by the top value, and pushes the result
divide :: Command ()
divide = binaryStackOp div

-- mod
-- Pops the top two values, calculates the second top value modulo the top value, and pushes the result
modop :: Command ()
modop = binaryStackOp mod

-- not
-- Replaces the top value of the stack with 0 if it is non-zero, and 1 if it is zero
notop :: Command ()
notop = do stack <- get
           case stack of
             (0:s) -> put $ 1:s
             (_:s) -> put $ 0:s

-- greater
-- Pops the top two values, pushes 1 on to the stack if the second top value is greater than the top value, 0 otherwise
greater :: Command ()
greater = do (x:y:stack) <- get
             case y > x of
               True -> put $ 1:stack
               False -> put $ 0:stack

end :: State Bool ()
end = put False

label :: String -> State ([Command a], [Command a]) String
label s = return s

goto :: Int -> State ([Command a], [Command a]) ()
goto x = do (a, b) <- get
            put $ splitAt x (a ++ b)

next :: State([Command a], [Command a]) (Command a)
next = do (a, (b:rest)) <- get
          put (a ++ [b], rest)
          return b
