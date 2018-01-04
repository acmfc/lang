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
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Lang.Core
import Lang.Expr
import Lang.Identifier
import qualified Lang.Type as T

-- Taken from:
-- https://stackoverflow.com/questions/17968784/an-easy-way-to-change-the-type-of-parsec-user-state
changeState :: (Functor m, Monad m)
            => (u -> v)
            -> (v -> u)
            -> ParsecT s u m a
            -> ParsecT s v m a
changeState forward backward = mkPT . transform . runParsecT
  where
    mapState f st = st { stateUser = f (stateUser st) }
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e
    fmap3 = fmap . fmap . fmap
    transform p st = fmap3 (mapReply forward) (p (mapState backward st))

-- TODO Properly handle operators with the operator parsers.

-- | Defines token rules for the type language
typeLanguageDef :: Token.LanguageDef TypeParserState
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

type TypeParserState = (Int, [T.Pred])
type TypeParser a = Parsec String TypeParserState a

typeTokenParser :: Token.GenTokenParser String TypeParserState Identity
typeTokenParser = Token.makeTokenParser typeLanguageDef

isTypeConstructor :: String -> Bool
isTypeConstructor [] = False
isTypeConstructor (c : _) = isUpper c

tAtom :: TypeParser T.Type
tAtom = do
    i <- Token.identifier typeTokenParser
    if isTypeConstructor i then return $ T.TCon $ T.Tycon i T.KStar
    else return $ T.TVar $ T.Tyvar i T.KStar

tFunction :: TypeParser T.Type
tFunction = do
    left <- try $ do
        left <- Token.parens typeTokenParser typ' <|> tRecord <|> tAtom
        _ <- Token.symbol typeTokenParser "->"
        return left
    right <- typ'
    return $ T.makeFun left right

constrainRowVarForLabel :: T.Row -> T.Row -> LabelName -> T.Type -> [T.Pred]
constrainRowVarForLabel extends rowVar labelName t = preds
  where
    preds = [ T.RowEq rowVar (T.RExt l t extends)
            , T.RowLacks extends l
            ]
    l = T.makeLabelType labelName

makeRowVariableName :: Int -> T.TypeVariableName
makeRowVariableName = ("$r" ++) . show

makeRowVarForLabels :: T.Row
                    -> Int
                    -> [(LabelName, T.Type)]
                    -> (Int, T.Row, [T.Pred])
makeRowVarForLabels baseRowVar startIdx = foldr f (startIdx, baseRowVar, [])
  where
    f (i, t) (n, extends, preds) = (n + 1, rowVar, preds' ++ preds)
      where
        rowVar = T.RVar $ T.TVar $ T.Tyvar (makeRowVariableName n) T.KRow
        preds' = constrainRowVarForLabel extends rowVar i t

tLabel :: TypeParser (LabelName, T.Type)
tLabel = do
    i <- Token.identifier typeTokenParser
    _ <- Token.symbol typeTokenParser ":"
    t <- typ'
    return (i, t)

tRecordBase :: TypeParser String
tRecordBase =
    Token.symbol typeTokenParser "|" >> Token.identifier typeTokenParser

tRecord' :: TypeParser T.Type
tRecord' = do
    ls <- sepBy tLabel (Token.symbol typeTokenParser ",")
    st <- getState
    let defaultBaseVarName = makeRowVariableName $ fst st
    baseVarName <- fromMaybe defaultBaseVarName <$> optionMaybe tRecordBase
    let baseVar = T.TVar $ T.Tyvar baseVarName T.KRow
        baseRowVar = T.RVar baseVar
        startIdx = fst st + 1
        (n, T.RVar rowVar, preds) = makeRowVarForLabels baseRowVar startIdx ls
    modifyState $ (,) n . (preds ++) . snd
    return $ T.TAp T.tRecordCon rowVar

tRecord :: TypeParser T.Type
tRecord = Token.braces typeTokenParser tRecord'

typ' :: TypeParser T.Type
typ' = tFunction <|> Token.parens typeTokenParser typ' <|> tRecord <|> tAtom

typ :: Parser T.Scheme
typ = T.genEmptyEnv . uncurry T.Qual <$> stateless
  where
    stateless = changeState (const ()) (const (0, [])) typ''
    typ'' = do
        t <- typ'
        st <- getState
        return (snd st, t)

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

