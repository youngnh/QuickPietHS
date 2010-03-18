-- These are the stack operations which follow the actions available with the Piet programming languages with some 
-- small changes to allow algorithms to be tested without the need of creating valid Piet images.
-- Original Piet information can be found at http://www.dangermouse.net/esoteric/piet.html
{-# LANGUAGE DeriveDataTypeable #-}
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
push elt stack= elt:stack

push2 :: Int -> State Stack ()
push2 elt = do stack <- get
               put (elt:stack)

-- pop
-- Pops the top value of the stack and discards
pop :: Stack -> Stack
pop (elt:stack) = stack

pop2 :: State Stack ()
pop2 = do (elt:stack) <- get
          put stack

-- duplicate
-- Pushes a copy of the top value of the stack onto the stack
duplicate :: Stack -> Stack
duplicate (x:stack) = x:x:stack

duplicate2 :: State Stack ()
duplicate2 = do (x:stack) <- get
                put (x:x:stack)

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

roll2 :: State Stack ()
roll2 = do (x:y:stack) <- get
           let (top, bot) = splitAt y stack
           put (roll' x top ++ bot)
    where roll' 0 lst = lst
          roll' n (elt:lst) = roll' (n - 1) (lst ++ [elt])

-- in
-- Read a single value from STDIN and push it onto the stack; characters are read as their ASCII value
inop :: String -> Stack -> (String, Stack)
inop bs stack = (cs, c:stack)
    where c = ord (head bs)
          cs = tail bs

inop2 :: Char -> State Stack ()
inop2 c = do stack <- get
             put $ ord c : stack

-- out
-- Pop the top value from the stack and append it's ASCII character value to STDOUT
outop :: String -> Stack -> (String, Stack)
outop bs (x:stack) = ((bs ++ [b]), stack)
    where b = chr x
outop _ [] = throw (StackException "Cannot pop to output from an empty stack")

outop2 :: State Stack Char
outop2 = do (c:stack) <- get
            put stack
            return (chr c)

binaryStackOp :: (Int -> Int -> Int) -> State Stack ()
binaryStackOp f = do (x:y:stack) <- get
                     put $ (f x y) : stack

-- add
-- Pops the top two values, adds them, and pushes the result
add :: Stack -> Stack
add (x:y:stack) = (x + y):stack

add2 :: State Stack ()
add2 = binaryStackOp (+)

-- subtract
-- Pops the top two values, subtracts the top value from the second top value, and pushes the result
subtractop :: Stack -> Stack
subtractop (x:y:stack) = (y - x):stack

subtractop2 :: State Stack ()
subtractop2 = binaryStackOp (-)

-- multiply
-- Pops the top two values, multiplies them, and pushes the result
multiply :: Stack -> Stack
multiply (x:y:stack) = (x * y):stack

multiply2 :: State Stack ()
multiply2 = binaryStackOp (*)

-- divide
-- Pops the top two values, integer divides the second top value by the top value, and pushes the result
divide :: Stack -> Stack
divide (x:y:stack) = (y `div` x):stack

divide2 :: State Stack ()
divide2 = binaryStackOp div

-- mod
-- Pops the top two values, calculates the second top value modulo the top value, and pushes the result
modop :: Stack -> Stack
modop (x:y:stack) = (y `mod` x):stack

modop2 :: State Stack ()
modop2 = binaryStackOp mod

-- not
-- Replaces the top value of the stack with 0 if it is non-zero, and 1 if it is zero
notop :: Stack -> Stack
notop (0:stack) = 1:stack
notop (_:stack) = 0:stack

notop2 :: State Stack ()
notop2 = do stack <- get
            case stack of
              (0:s) -> put $ 1:s
              (_:s) -> put $ 0:s

-- greater
-- Pops the top two values, pushes 1 on to the stack if the second top value is greater than the top value, 0 otherwise
greater :: Stack -> Stack
greater (x:y:stack)
    | y > x = 1:stack
    | otherwise = 0:stack

greater2 :: State Stack ()
greater2 = do (x:y:stack) <- get
              case y > x of
                True -> put $ 1:stack
                False -> put $ 0:stack