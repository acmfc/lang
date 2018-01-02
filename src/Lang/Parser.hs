module Lang.Parser
    ( typ
    , var
    , literal
    , application
    , expr
    , term
    , binding
    , program
    , parseProgram
    ) where

import Data.Char (isUpper)
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Lang.Core
import Lang.Expr
import Lang.Identifier
import qualified Lang.Type as T

-- | Defines token rules for the type language
typeLanguageDef :: Token.LanguageDef ()
typeLanguageDef = Token.LanguageDef
    { Token.commentStart = ""
    , Token.commentEnd = ""
    , Token.commentLine = ""
    , Token.nestedComments = False
    , Token.identStart = letter <|> char '_'
    , Token.identLetter = alphaNum <|> char '_'
    , Token.opStart = oneOf ".=->()"
    , Token.opLetter = oneOf ".=->()"
    , Token.reservedNames = ["forall"]
    , Token.reservedOpNames = [".", "->", "(", ")"]
    , Token.caseSensitive = True
    }

typeTokenParser :: Token.GenTokenParser String () Identity
typeTokenParser = Token.makeTokenParser typeLanguageDef

isTypeConstructor :: String -> Bool
isTypeConstructor [] = False
isTypeConstructor (c : _) = isUpper c

tAtom :: Parser T.Type
tAtom = do
    i <- Token.identifier typeTokenParser
    if isTypeConstructor i then return $ T.TCon $ T.Tycon i T.KStar
    else return $ T.TVar $ T.Tyvar i T.KStar

tFunction :: Parser T.Type
tFunction = do
    left <- try $ do
        left <- Token.parens typeTokenParser typ' <|> tAtom
        _ <- Token.symbol typeTokenParser "->"
        return left
    right <- typ'
    return $ T.makeFun left right

typ' :: Parser T.Type
typ' = tFunction <|> Token.parens typeTokenParser typ' <|> tAtom

typ :: Parser T.Scheme
typ = T.genEmptyEnv . T.Qual [] <$> typ'

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
    , Token.reservedNames = ["val", "let", "fn"]
    , Token.reservedOpNames = ["=", "@", "->"]
    , Token.caseSensitive = True
    }

tokenParser :: Token.GenTokenParser String () Identity
tokenParser = Token.makeTokenParser languageDef

-- | Variable parser.
var :: Parser SyntacticExpr
var = evar <$> Token.identifier tokenParser

-- | Literal value parser.
literal :: Parser SyntacticExpr
literal = int <|> constLabel

int :: Parser SyntacticExpr
int = elit . LInt <$> Token.integer tokenParser

constLabel :: Parser SyntacticExpr
constLabel = do
    _ <- Token.symbol tokenParser "@"
    l <- Token.identifier tokenParser
    return $ elit . LLab $ l

term :: Parser SyntacticExpr
term = var <|> literal

-- | Function application parser.
application :: Parser SyntacticExpr
application = Token.lexeme tokenParser $ chainl1 expr' f
  where
    f = return eap
    --Token.lexeme tokenParser $ chainl1 expr' $ return eap

-- | Expression parser.
expr :: Parser SyntacticExpr
expr = application <|> term

-- | Parser for expressions that can be found in a function application.
-- Further applications must be enclosed in parentheses to avoid infinite
-- recursion.
expr' :: Parser SyntacticExpr
expr' = Token.parens tokenParser application <|> term

-- | Type declaration using val.
valtype :: Parser (VariableName, T.Scheme)
valtype = do
    _ <- Token.reserved tokenParser "val"
    boundId <- Token.identifier tokenParser
    _ <- Token.symbol tokenParser ":"
    t <- typ
    return (boundId, t)

-- | Let binding parser.
binding :: Parser (Binding (Maybe T.Scheme) SyntacticExpr)
binding = do
    decl <- optionMaybe valtype
    let t = fmap snd decl
    _ <- Token.reserved tokenParser "let"
    boundId <- Token.identifier tokenParser
    args <- many $ Token.identifier tokenParser
    _ <- Token.symbol tokenParser "="
    e <- expr
    return Binding { identifier = boundId
                   , arguments = args
                   , body = e
                   , annot = t
                   }

-- | Parser for a program as a sequence of bindings.
program :: Parser (BindingGroup (Maybe T.Scheme) SyntacticExpr)
program = many binding

parseProgram :: String ->
    Either ParseError (BindingGroup (Maybe T.Scheme) SyntacticExpr)
parseProgram = parse program ""

