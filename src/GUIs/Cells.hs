module GUIs.Cells
    ( cells
    ) where

import           Reflex
import           Reflex.Dom

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
import Text.Parsec.String (Parser)

data BinOp
    = Plus
    | Minus
    | Times
    | Div
    deriving (Show)

data UnOp
    = Negate
    deriving (Show)

data Expr
    = ERef Integer Integer
    | EBinOp BinOp Expr Expr
    | EUnOp UnOp Expr
    | ENumber Double
    deriving (Show)

lexer = P.makeTokenParser emptyDef
    { P.reservedOpNames = ["+", "-", "*", "/"]
    }

parens = P.parens lexer
braces = P.braces lexer
float = P.float lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
natural = P.natural lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer

number :: Parser Expr
number = ENumber <$> float

ref :: Parser Expr
ref = braces $ do
    i <- natural
    lexeme $ char ','
    j <- natural
    pure $ ERef i j

binary name op = Infix (reservedOp name *> pure (EBinOp op)) AssocLeft
prefix name op = Prefix $ reservedOp name *> pure (EUnOp op)

expr = buildExpressionParser table terms
  where
    table =
        [ [ prefix "-" Negate ]
        , [ binary "*" Times, binary "/" Div ]
        , [ binary "+" Plus, binary "-" Minus ]
        ]
    terms = parens expr
        <|> ref
        <|> number
        <?> "term"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace *> expr <* eof) ""

cells :: MonadWidget t m => m ()
cells = el "div" $ do
    raw <- textInput def
    eExpr <- mapDyn (show . parseExpr) $ _textInput_value raw
    dynText eExpr
