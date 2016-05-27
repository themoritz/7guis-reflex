module GUIs.Cells.Parser
    ( parseExpression
    ) where

import           Data.Decimal

import           Control.Monad.Identity

import           Text.Parsec
import qualified Text.Parsec.Token    as P
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import           Text.Read            (readMaybe)

import           GUIs.Cells.Types

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
    { P.reservedOpNames = ["+", "-", "*", "/"]
    }

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

number :: Parser Expr
number = lexeme $ do
    raw <- many $ oneOf "0123456789."
    case readMaybe raw of
        Nothing -> fail "expected decimal number"
        Just dec -> pure $ ENumber dec

ref :: Parser Expr
ref = (P.braces lexer) $ do
    i <- P.natural lexer
    lexeme $ char ','
    j <- P.natural lexer
    pure $ ERef $ Coords (fromInteger i) (fromInteger j)

binary :: String -> BinOp -> Operator String () Identity Expr
binary name op = Infix (reservedOp name *> pure (EBinOp op)) AssocLeft

prefix :: String -> UnOp -> Operator String () Identity Expr
prefix name op = Prefix $ reservedOp name *> pure (EUnOp op)

expression :: Parser Expr
expression = buildExpressionParser table terms
  where
    table =
        [ [ prefix "-" Negate ]
        , [ binary "*" Times, binary "/" Div ]
        , [ binary "+" Plus, binary "-" Minus ]
        ]
    terms = (P.parens lexer) expression
        <|> ref
        <|> number
        <?> "term"

parseExpression :: String -> Either ParseError Expr
parseExpression = parse ((P.whiteSpace lexer) *> expression <* eof) ""
