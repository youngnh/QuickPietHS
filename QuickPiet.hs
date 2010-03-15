-- file: QuickPiet.hs
-- An implementation of QuickPiet by Ben Lee
-- based on the Piet language by ???

-- These are the stack operations which follow the actions available with the Piet programming languages with some 
-- small changes to allow algorithms to be tested without the need of creating valid Piet images.
-- Original Piet information can be found at http://www.dangermouse.net/esoteric/piet.html

-- Just as in Piet, this language spec assumes a single "infinite" stack and a linear command execution order.
-- Blank lines should be ignored.
-- An implicit "end" command is present at the bottom of the document.


-- file: Piet.hs
-- An Interpreter for Ben's Piet Challenge
import qualified Data.ByteString.Lazy as L

-- push X 
-- Pushes the value of X onto the stack.  X should be a positive integer
push :: Int -> [Int] -> [Int]
push elt stack = elt:stack

-- pop
-- Pops the top value of the stack and discards
pop :: [Int] -> [Int]
pop (elt:stack) = stack

-- duplicate
-- Pushes a copy of the top value of the stack onto the stack
duplicate :: [Int] -> [Int]
duplicate (x:stack) = x:x:stack

-- roll
-- Pops the top two values, and "rolls" the remaining stack entries to a depth equal to the second value popped ...
-- By a number of rolls equal to the first value popped ...
-- A single roll to depth n is defined as burying the top value on the stack n deep ...
-- And bringing all values above it up by 1 place ...
-- A negative number of rolls rolls in the opposite direction
roll :: [Int] -> [Int]
roll (x:y:stack) = (roll' x top) ++ bot
    where (top, bot) = splitAt y stack
          roll' 0 lst = lst
          roll' n (elt:lst) = roll' (n - 1) (lst ++ [elt])

-- in
-- Read a single value from STDIN and push it onto the stack; characters are read as their ASCII value
inop :: L.ByteString -> [Int] -> (L.ByteString, [Int])
inop bs stack = (cs, (c:stack))
    where c = fromIntegral (L.head bs)
          cs = L.tail bs

-- out
-- Pop the top value from the stack and output it to STDOUT in it's ASCII character value
outop :: L.ByteString -> [Int] -> (L.ByteString, [Int])
outop bs (x:stack) = ((L.cons b bs), stack)
    where b = fromIntegral x

-- add
-- Pops the top two values, adds them, and pushes the result
add :: [Int] -> [Int]
add (x:y:stack) = (x + y):stack

-- subtract
-- Pops the top two values, subtracts the top value from the second top value, and pushes the result
subtract :: [Int] -> [Int]
subtract (x:y:stack) = (y - x):stack

-- multiply
-- Pops the top two values, multiplies them, and pushes the result
multiply :: [Int] -> [Int]
multiply (x:y:stack) = (x * y):stack

-- divide
-- Pops the top two values, integer divides the second top value by the top value, and pushes the result
divide :: [Int] -> [Int]
divide (x:y:stack) = (y `div` x):stack

-- mod
-- Pops the top two values, calculates the second top value modulo the top value, and pushes the result
modop :: [Int] -> [Int]
modop (x:y:stack) = (y `mod` x):stack

-- not
-- Replaces the top value of the stack with 0 if it is non-zero, and 1 if it is zero
not :: [Int] -> [Int]
not (0:stack) = 1:stack
not (_:stack) = 0:stack

-- greater
-- Pops the top two values, pushes 1 on to the stack if the second top value is greater than the top value, 0 otherwise
greater :: [Int] -> [Int]
greater (x:y:stack)
    | y > x = 1:stack
    | otherwise = 0:stack

-- end
-- Stop program execution, values left on the stack are discarded
end :: [Int] -> [Int]
end _ = []

-- Comments start with #

-- :label
-- Line label must begin with a ":" character and at least one alpha-numeric character

-- goto label label
-- Pops the top value from the stack ...
-- If the value is equal to 1, program execution switches to the first label ...
-- If the value equals 3, program execution switches to the second label ...
-- If the value does not equal 1 or 3, program execution continues to the next line
