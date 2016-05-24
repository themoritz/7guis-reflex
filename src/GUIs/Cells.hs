module GUIs.Cells
    ( cells
    ) where

import           Reflex
import           Reflex.Dom

import Data.Either

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
import Text.Parsec.String (Parser)

data Ref
    = Ref Integer Integer
    deriving (Show)

-- Expressions

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
    = ERef Ref
    | EBinOp BinOp Expr Expr
    | EUnOp UnOp Expr
    | ENumber Double
    | EEmpty
    deriving (Show)

-- Parser

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
    pure $ ERef $ Ref i j

empty :: Parser Expr
empty = pure EEmpty

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
parseExpr = parse (whiteSpace *> (expr <|> empty) <* eof) ""

-- Evaluator

data EvalError
    = EvalRefNotFound Ref
    | EvalDivByZero
    deriving (Show)

-- Single cell of the spreadsheet

data CellResult
    = CellErrorParse ParseError
    | CellErrorEval EvalError
    | CellResult Double
    | CellEmpty
    deriving (Show)

cell :: MonadWidget t m
     => Event t (Either EvalError Double)
     -> m (Event t Expr)
cell evalEv = el "div" $ do
    raw <- textInput def
    let eExpr = parseExpr <$> _textInput_input raw
        events  = leftmost
          [ either CellErrorEval CellResult <$> evalEv
          , fmapMaybe (either (Just . CellErrorParse) (const Nothing)) eExpr
          ]
    cellResult <- holdDyn CellEmpty events
    cellResultText <- mapDyn show cellResult
    dynText cellResultText
    pure $ fmapMaybe (either (const Nothing) Just) eExpr

cells :: MonadWidget t m => m ()
cells = el "div" $ do
  expr <- cell never
  dynExpr <- holdDyn "" (show <$> expr)
  dynText dynExpr
  pure ()
