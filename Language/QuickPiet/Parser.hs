module Language.QuickPiet.Parser 
    (parseScript
    ) where

import Language.QuickPiet.StackOperations (Command(..))
import Text.ParserCombinators.Parsec hiding (label)

-- a script is a bunch of lines terminated by an EOF
script :: GenParser Char st [Command]
script = do result <- many line
            eof
            return result

-- a line contains a single command terminated by a eol (newline)
line :: GenParser Char st Command
line = do result <- command
          eol
          return result

-- a eol is a single \n char
eol :: GenParser Char st Char
eol = char '\n'

-- a command is a comment a label or an action
command :: GenParser Char st Command
command = comment <|> label <|> action

-- a comment is a # followed by zero or more chars
comment :: GenParser Char st Command
comment = do char '#'
             many (noneOf "\n")
             return Comment

-- a label is a : followed by zero or more alpha-numeric chars
label :: GenParser Char st Command
label = do char ':'
           name <- many alphaNum
           return (Label name)

action :: GenParser Char st Command
action = push 
         <|> pop 
         <|> duplicate 
         <|> roll 
         <|> inop 
         <|> outop
         <|> add
         <|> subtractop 
         <|> multiply 
         <|> divide 
         <|> modop
         <|> notop
         <|> greater
         <|> end
         <|> goto

push :: GenParser Char st Command
push = do string "push"
          x <- many digit
          return (Push (read x))

pop = do string "pop"
         return Pop

duplicate = do string "duplicate"
               return Duplicate

roll = do string "roll"
          return Roll

inop = do string "in"
          return In

outop = do string "out"
           return Out

add = do string "add"
         return Add

subtractop = do string "subtract"
                return Subtract

multiply = do string "multiply"
              return Multiply

divide = do string "divide"
            return Divide

modop = do string "mod"
           return Mod

notop = do string "not"
           return Not

greater = do string "greater"
             return Greater

end = do string "end"
         return End

goto = do string "goto"
          label <- many alphaNum
          char ' '
          other <- many alphaNum
          return (Goto label other)

parseScript :: String -> Either ParseError [Command]
parseScript = parse script "(unknown)"

