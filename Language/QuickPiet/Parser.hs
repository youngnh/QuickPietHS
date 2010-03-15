module Language.QuickPiet.Parser 
    (parseQP
    ) where

import Language.QuickPiet.StackOperations
import Text.ParserCombinators.Parsec

-- a script is a bunch of lines terminated by an EOF
qpScript :: GenParser Char st [Command]
qpScript = do result <- many line
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
command = comment <|> qpLabel <|> action

-- a comment is a # followed by zero or more chars
comment :: GenParser Char st Command
comment = do char '#'
             many (noneOf "\n")
             return Comment

-- a label is a : followed by zero or more alpha-numeric chars
qpLabel :: GenParser Char st Command
qpLabel = do char ':'
             name <- many alphaNum
             return (Label name)

action :: GenParser Char st Command
action = qpPush 
         <|> qpPop 
         <|> qpDuplicate 
         <|> qpRoll 
         <|> qpIn 
         <|> qpOut 
         <|> qpAdd 
         <|> qpSubtract 
         <|> qpMultiply 
         <|> qpDivide 
         <|> qpMod
         <|> qpNot
         <|> qpGreater
         <|> qpEnd
         <|> qpGoto

qpPush :: GenParser Char st Command
qpPush = do string "push"
            x <- many digit
            return (Push (read x))

qpPop = do string "pop"
           return Pop

qpDuplicate = do string "duplicate"
                 return Duplicate

qpRoll = do string "roll"
            return Roll

qpIn = do string "in"
          return In

qpOut = do string "out"
           return Out

qpAdd = do string "add"
           return Add

qpSubtract = do string "subtract"
                return Subtract

qpMultiply = do string "multiply"
                return Multiply

qpDivide = do string "divide"
              return Divide

qpMod = do string "mod"
           return Mod

qpNot = do string "not"
           return Not

qpGreater = do string "greater"
               return Greater

qpEnd = do string "end"
           return End

qpGoto = do string "goto"
            label <- many alphaNum
            char ' '
            other <- many alphaNum
            return (Goto label other)

parseQP :: String -> Either ParseError [Command]
parseQP input = parse qpScript "(unknown)" input

