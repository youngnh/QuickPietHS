-- file: Piet.hs
-- An Interpreter for Ben's Piet Challenge
import qualified Data.ByteString.Lazy as L

push :: Int -> [Int] -> [Int]
push elt stack = elt:stack

pop :: [Int] -> [Int]
pop (elt:stack) = stack

duplicate :: [Int] -> [Int]
duplicate (x:stack) = x:x:stack

roll :: [Int] -> [Int]
roll (x:y:stack) = (roll' x top) ++ bot
    where (top, bot) = splitAt y stack
          roll' 0 lst = lst
          roll' n (elt:lst) = roll' (n - 1) (lst ++ [elt])

inop :: L.ByteString -> [Int] -> (L.ByteString, [Int])
inop bs stack = (cs, (c:stack))
    where c = fromIntegral (L.head bs)
          cs = L.tail bs

outop :: L.ByteString -> [Int] -> (L.ByteString, [Int])
outop bs (x:stack) = ((L.cons b bs), stack)
    where b = fromIntegral x

add :: [Int] -> [Int]
add (x:y:stack) = (x + y):stack

subtract :: [Int] -> [Int]
subtract (x:y:stack) = (y - x):stack

multiply :: [Int] -> [Int]
multiply (x:y:stack) = (x * y):stack

divide :: [Int] -> [Int]
divide (x:y:stack) = (y `div` x):stack

modop :: [Int] -> [Int]
modop (x:y:stack) = (y `mod` x):stack

not :: [Int] -> [Int]
not (0:stack) = 1:stack
not (_:stack) = 0:stack

greater :: [Int] -> [Int]
greater (x:y:stack)
    | y > x = 1:stack
    | otherwise = 0:stack

end :: [Int] -> [Int]
end _ = []