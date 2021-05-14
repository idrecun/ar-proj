module ParseLogic where

import Logic

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

{-
 - Lekser
 -}

language = emptyDef { Token.identStart = letter,
                      Token.identLetter = alphaNum,
                      Token.reservedNames = ["true", "false"],
                      Token.reservedOpNames = ["&", "|", "=>", "<=>", "~"] }

lexer = Token.makeTokenParser language

-- Lekseme
operator   = Token.reservedOp lexer
constant   = Token.reserved lexer
brackets   = Token.parens lexer
identifier = Token.identifier lexer


{-
 - Parser
 -}

operators = [ [Prefix (operator   "~" >> return Not)                      ],
              [Infix  (operator   "&" >> return (Binary And))  AssocLeft  ],
              [Infix  (operator   "|" >> return (Binary Or))   AssocLeft  ],
              [Infix  (operator  "=>" >> return (Binary Impl)) AssocRight ],
              [Infix  (operator "<=>" >> return (Binary Eql))  AssocRight ] ]

atom =  brackets expr
    <|> (constant "true" >> return LogicT)
    <|> (constant "false" >> return LogicF)
    <|> liftM Atom identifier

expr :: Parser Logic
expr = buildExpressionParser operators atom

parser :: String -> Logic
parser str = case result of
              Left err -> error (show err)
              Right l  -> l
             where result = parse expr "" str
