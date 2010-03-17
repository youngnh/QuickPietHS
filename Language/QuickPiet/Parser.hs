module Language.QuickPiet.Parser 
    (parseScript
    ) where

import Language.QuickPiet.StackOperations (Command(..))
import Text.ParserCombinators.Parsec hiding (label)
import qualified Text.ParserCombinators.Parsec.Token as T

quickPietDef = T.LanguageDef {
                 T.commentStart = ""
               , T.commentEnd = ""
               , T.commentLine = "#"
               , T.nestedComments = False
               , T.identStart = alphaNum
               , T.identLetter = alphaNum
               , T.opStart = pzero
               , T.opLetter = anyChar
               , T.reservedNames = []
               , T.reservedOpNames = []
               , T.caseSensitive = True
               }

lexer = T.makeTokenParser quickPietDef
whiteSpace = T.whiteSpace lexer
identifier = T.identifier lexer
symbol = T.symbol lexer
natural = T.natural lexer

-- a script is a bunch of lines terminated by an EOF
script :: GenParser Char st [Command]
script = do whiteSpace
            result <- many command
            eof
            return result

-- a eol is a single \n char
eol :: GenParser Char st Char
eol = char '\n'

-- a command is a comment a label or an action
command :: GenParser Char st Command
command = label <|> action

-- a label is a : followed by an identifier
label :: GenParser Char st Command
label = do char ':'
           name <- identifier
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
push = do symbol "push"
          x <- natural
          return (Push (fromIntegral x))

instruction :: String -> Command -> GenParser Char st Command
instruction s c = do symbol s
                     return c

goto = do symbol "goto"
          label <- identifier
          other <- identifier
          return (Goto label other)

parseScript :: SourceName -> String -> Either ParseError [Command]
parseScript = parse script

