module Language.QuickPiet.Parser 
    (parseScript
    ) where

import Language.QuickPiet.StackOperations (Command(..))
import Text.ParserCombinators.Parsec hiding (label)

-- a script is a bunch of lines terminated by an EOF
script :: GenParser Char st [Command]
script = do result <- sepEndBy command eol
            eof
            return result

-- a eol is a single \n char
eol :: GenParser Char st Char
eol = char '\n'

-- a command is a comment a label or an action
command :: GenParser Char st Command
command = comment <|> label <|> action <|> blank

-- a blank line is all whitespace
blank = do many (oneOf " \t")
           return Blank

-- a comment is a # followed by zero or more chars
comment :: GenParser Char st Command
comment = do char '#'
             text <- many (noneOf "\n")
             return (Comment text)

-- a label is a : followed by zero or more alpha-numeric chars
label :: GenParser Char st Command
label = do char ':'
           name <- many alphaNum
           return (Label name)

action :: GenParser Char st Command
action = push 
         <|> instruction "pop" Pop
         <|> instruction "duplicate" Duplicate
         <|> instruction "roll" Roll
         <|> instruction "in" In
         <|> instruction "out" Out
         <|> instruction "add" Add
         <|> instruction "subtract" Subtract
         <|> instruction "multiply" Multiply
         <|> instruction "divide" Divide
         <|> instruction "mod" Mod
         <|> instruction "not" Not
         <|> instruction "greater" Greater
         <|> instruction "end" End
         <|> goto

push :: GenParser Char st Command
push = do string "push"
          char ' '
          x <- many digit
          return (Push (read x))

instruction :: String -> Command -> GenParser Char st Command
instruction s c = do string s
                     return c

goto = do string "goto"
          label <- many alphaNum
          char ' '
          other <- many alphaNum
          return (Goto label other)

parseScript :: SourceName -> String -> Either ParseError [Command]
parseScript = parse script

