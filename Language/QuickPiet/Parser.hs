module Language.QuickPiet.Parser where

import qualified Language.QuickPiet.StackOperations as Op
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

action :: GenParser Char st (Op.Command ())
action = push 
         <|> instruction "pop" Op.pop
         <|> instruction "duplicate" Op.duplicate
         <|> instruction "roll" Op.roll
         <|> instruction "add" Op.add
         <|> instruction "subtract" Op.subtractop
         <|> instruction "multiply" Op.multiply
         <|> instruction "divide" Op.divide
         <|> instruction "mod" Op.modop
         <|> instruction "not" Op.notop
         <|> instruction "greater" Op.greater

push :: GenParser Char st (Op.Command ())
push = try $ do symbol "push"
                x <- natural
                return $ Op.push (fromIntegral x)

instruction :: String -> (Op.Command ()) -> GenParser Char st (Op.Command ())
instruction s c = try $ do symbol s
                           return c

parseAction :: String -> Either ParseError (Op.Command ())
parseAction = parse action ""