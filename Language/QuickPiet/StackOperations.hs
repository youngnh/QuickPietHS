-- These are the stack operations which follow the actions available with the Piet programming languages with some 
-- small changes to allow algorithms to be tested without the need of creating valid Piet images.
-- Original Piet information can be found at http://www.dangermouse.net/esoteric/piet.html
{-# LANGUAGE DeriveDataTypeable #-}
module Language.QuickPiet.StackOperations 
    (Command(..)
    ,Stack(..)
    ,push
    ,pop
    ,duplicate
    ,roll
    ,inop
    ,outop
    ,add
    ,subtractop
    ,multiply
    ,divide
    ,modop
    ,notop
    ,greater
    )where

import Control.Exception
import Data.Char
import Data.Typeable

newtype Stack = Stack [Int]

data StackException = StackException String
                      deriving (Typeable)

instance Exception StackException
instance Show StackException where
    show (StackException s) = s

instance Show Stack where
    show (Stack []) = "$"
    show (Stack (x:xs)) = (show x) ++ " " ++ (show (Stack xs))

data Command = Push Int
             | Pop
             | Duplicate
             | Roll
             | In
             | Out
             | Add
             | Subtract
             | Multiply
             | Divide
             | Mod
             | Not
             | Greater
             | End
             | Comment String
             | Blank
             | Label String
             | Goto String String
               deriving (Eq)

instance Show Command where
    show (Push x) = "push " ++ (show x)
    show Pop = "pop"
    show Duplicate = "duplicate"
    show Roll = "roll"
    show In = "in"
    show Out = "out"
    show Add = "add"
    show Subtract = "subtract"
    show Multiply = "multiply"
    show Divide = "divide"
    show Mod = "mod"
    show Not = "not"
    show Greater = "greater"
    show End = "end"
    show (Comment s) = "#" ++ s
    show (Label s) = ":" ++ s
    show (Goto label other) = "goto " ++ label ++ " " ++ other

-- push X
-- Pushes the value of X onto the stack.  X should be a positive integer
push :: Int -> Stack -> Stack
push elt (Stack stack)= (Stack (elt:stack))

-- pop
-- Pops the top value of the stack and discards
pop :: Stack -> Stack
pop (Stack (elt:stack)) = (Stack stack)

-- duplicate
-- Pushes a copy of the top value of the stack onto the stack
duplicate :: Stack -> Stack
duplicate (Stack (x:stack)) = (Stack (x:x:stack))

-- roll
-- Pops the top two values, and "rolls" the remaining stack entries to a depth equal to the second value popped ...
-- By a number of rolls equal to the first value popped ...
-- A single roll to depth n is defined as burying the top value on the stack n deep ...
-- And bringing all values above it up by 1 place ...
-- A negative number of rolls rolls in the opposite direction
roll :: Stack -> Stack
roll (Stack (x:y:stack)) = (Stack (roll' x top ++ bot))
    where (top, bot) = splitAt y stack
          roll' 0 lst = lst
          roll' n (elt:lst) = roll' (n - 1) (lst ++ [elt])

-- in
-- Read a single value from STDIN and push it onto the stack; characters are read as their ASCII value
inop :: String -> Stack -> (String, Stack)
inop bs (Stack stack) = (cs, (Stack (c:stack)))
    where c = ord (head bs)
          cs = tail bs

-- out
-- Pop the top value from the stack and append it's ASCII character value to STDOUT
outop :: String -> Stack -> (String, Stack)
outop bs (Stack (x:stack)) = ((bs ++ [b]), (Stack stack))
    where b = chr x
outop _ (Stack []) = throw (StackException "Cannot pop to output from an empty stack")

-- add
-- Pops the top two values, adds them, and pushes the result
add :: Stack -> Stack
add (Stack (x:y:stack)) = (Stack ((x + y):stack))

-- subtract
-- Pops the top two values, subtracts the top value from the second top value, and pushes the result
subtractop :: Stack -> Stack
subtractop (Stack (x:y:stack)) = (Stack ((y - x):stack))

-- multiply
-- Pops the top two values, multiplies them, and pushes the result
multiply :: Stack -> Stack
multiply (Stack (x:y:stack)) = (Stack ((x * y):stack))

-- divide
-- Pops the top two values, integer divides the second top value by the top value, and pushes the result
divide :: Stack -> Stack
divide (Stack (x:y:stack)) = (Stack ((y `div` x):stack))

-- mod
-- Pops the top two values, calculates the second top value modulo the top value, and pushes the result
modop :: Stack -> Stack
modop (Stack (x:y:stack)) = (Stack ((y `mod` x):stack))

-- not
-- Replaces the top value of the stack with 0 if it is non-zero, and 1 if it is zero
notop :: Stack -> Stack
notop (Stack (0:stack)) = (Stack (1:stack))
notop (Stack (_:stack)) = (Stack (0:stack))

-- greater
-- Pops the top two values, pushes 1 on to the stack if the second top value is greater than the top value, 0 otherwise
greater :: Stack -> Stack
greater (Stack (x:y:stack))
    | y > x = (Stack (1:stack))
    | otherwise = (Stack (0:stack))
