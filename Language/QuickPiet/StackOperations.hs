-- These are the stack operations which follow the actions available with the Piet programming languages with some 
-- small changes to allow algorithms to be tested without the need of creating valid Piet images.
-- Original Piet information can be found at http://www.dangermouse.net/esoteric/piet.html

-- Just as in Piet, this language spec assumes a single "infinite" stack and a linear command execution order.
-- Blank lines should be ignored.
-- An implicit "end" command is present at the bottom of the document.

module Language.QuickPiet.StackOperations 
    (Command(..)
    ,Stack
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

import Data.Char

type Stack = [Int]

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
             | Comment
             | Label String
             | Goto String String
               deriving (Eq, Show)

-- push X
-- Pushes the value of X onto the stack.  X should be a positive integer
push :: Int -> Stack -> Stack
push elt stack = elt:stack

-- pop
-- Pops the top value of the stack and discards
pop :: Stack -> Stack
pop (elt:stack) = stack

-- duplicate
-- Pushes a copy of the top value of the stack onto the stack
duplicate :: Stack -> Stack
duplicate (x:stack) = x:x:stack

-- roll
-- Pops the top two values, and "rolls" the remaining stack entries to a depth equal to the second value popped ...
-- By a number of rolls equal to the first value popped ...
-- A single roll to depth n is defined as burying the top value on the stack n deep ...
-- And bringing all values above it up by 1 place ...
-- A negative number of rolls rolls in the opposite direction
roll :: Stack -> Stack
roll (x:y:stack) = roll' x top ++ bot
    where (top, bot) = splitAt y stack
          roll' 0 lst = lst
          roll' n (elt:lst) = roll' (n - 1) (lst ++ [elt])

-- in
-- Read a single value from STDIN and push it onto the stack; characters are read as their ASCII value
inop :: String -> Stack -> (String, Stack)
inop bs stack = (cs, (c:stack))
    where c = ord (head bs)
          cs = tail bs

-- out
-- Pop the top value from the stack and output it to STDOUT in it's ASCII character value
outop :: String -> Stack -> (String, Stack)
outop bs (x:stack) = ((b:bs), stack)
    where b = chr x

-- add
-- Pops the top two values, adds them, and pushes the result
add :: Stack -> Stack
add (x:y:stack) = (x + y):stack

-- subtract
-- Pops the top two values, subtracts the top value from the second top value, and pushes the result
subtractop :: Stack -> Stack
subtractop (x:y:stack) = (y - x):stack

-- multiply
-- Pops the top two values, multiplies them, and pushes the result
multiply :: Stack -> Stack
multiply (x:y:stack) = (x * y):stack

-- divide
-- Pops the top two values, integer divides the second top value by the top value, and pushes the result
divide :: Stack -> Stack
divide (x:y:stack) = (y `div` x):stack

-- mod
-- Pops the top two values, calculates the second top value modulo the top value, and pushes the result
modop :: Stack -> Stack
modop (x:y:stack) = (y `mod` x):stack

-- not
-- Replaces the top value of the stack with 0 if it is non-zero, and 1 if it is zero
notop :: Stack -> Stack
notop (0:stack) = 1:stack
notop (_:stack) = 0:stack

-- greater
-- Pops the top two values, pushes 1 on to the stack if the second top value is greater than the top value, 0 otherwise
greater :: Stack -> Stack
greater (x:y:stack)
    | y > x = 1:stack
    | otherwise = 0:stack
