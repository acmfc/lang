module Lang.Parser
    ( var
    , literal
    , application
    , expr
    , term
    , binding
    , program
    ) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Lang.Core

-- | Defines token rules for the language.
languageDef :: Token.LanguageDef ()
languageDef = Token.LanguageDef
    { Token.commentStart = ""
    , Token.commentEnd = ""
    , Token.commentLine = "#"
    , Token.nestedComments = False
    , Token.identStart = letter <|> char '_'
    , Token.identLetter = alphaNum <|> char '_'
    , Token.opStart = oneOf "~!@#$%^&*-=+.<>/?@\\|:"
    , Token.opLetter = oneOf "~!@#$%^&*-=+.<>/?@\\|:"
    , Token.reservedNames = ["let"]
    , Token.reservedOpNames = ["="]
    , Token.caseSensitive = True
    }

tokenParser :: Token.GenTokenParser String () Identity
tokenParser = Token.makeTokenParser languageDef

-- | Parses a variable.
var :: Parser Expr
var = EVar <$> Token.identifier tokenParser

-- | Parses a literal value.
literal :: Parser Expr
literal = ELit . LInt <$> Token.integer tokenParser

term :: Parser Expr
term = var <|> literal

-- | Parses a function application.
application :: Parser Expr
application = Token.lexeme tokenParser $ chainl1 expr' (return EAp)

-- | Parses an expression.
expr :: Parser Expr
expr = application <|> term

-- | Parses expressions that can be found in a function application. Further
-- applications must be enclosed in parentheses to avoid infinite recursion.
expr' :: Parser Expr
expr' = Token.parens tokenParser application <|> term

-- | Parses a let binding.
binding :: Parser Binding
binding = do
    _ <- Token.reserved tokenParser "let"
    boundId <- Token.identifier tokenParser
    args <- many $ Token.identifier tokenParser
    _ <- Token.symbol tokenParser "="
    e <- expr
    return $ Binding {identifier = boundId, arguments = args, body = e}

-- | Parses a program as a sequence of bindings.
program :: Parser BindingGroup
program = many binding

