module Lang.Parser
    ( var
    , literal
    , application
    , expr
    , term
    , binding
    , program
    , parseProgram
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

-- | Variable parser.
var :: Parser Expr
var = EVar <$> Token.identifier tokenParser

-- | Literal value parser.
literal :: Parser Expr
literal = ELit . LInt <$> Token.integer tokenParser

term :: Parser Expr
term = var <|> literal

-- | Function application parser.
application :: Parser Expr
application = Token.lexeme tokenParser $ chainl1 expr' (return EAp)

-- | Expression parser.
expr :: Parser Expr
expr = application <|> term

-- | Parser for expressions that can be found in a function application.
-- Further applications must be enclosed in parentheses to avoid infinite
-- recursion.
expr' :: Parser Expr
expr' = Token.parens tokenParser application <|> term

-- | Let binding parser.
binding :: Parser Binding
binding = do
    _ <- Token.reserved tokenParser "let"
    boundId <- Token.identifier tokenParser
    args <- many $ Token.identifier tokenParser
    _ <- Token.symbol tokenParser "="
    e <- expr
    return $ Binding {identifier = boundId, arguments = args, body = e}

-- | Parser for a program as a sequence of bindings.
program :: Parser BindingGroup
program = many binding

parseProgram :: String -> Either ParseError BindingGroup
parseProgram = parse program ""

