{-# LANGUAGE RecursiveDo #-}

module GUIs.Cells
    ( cells
    ) where

import           Reflex
import           Reflex.Dom

import Control.Monad (when)

import Text.Read (readMaybe)
import Data.Decimal
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec hiding (Empty)
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import GUIs.Cells.Sheet
import GUIs.Cells.Types

import Utils

-- Parser

lexer = P.makeTokenParser emptyDef
    { P.reservedOpNames = ["+", "-", "*", "/"]
    }

parens = P.parens lexer
braces = P.braces lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
natural = P.natural lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer

number :: Parser Expr
number = lexeme $ do
    raw <- many $ oneOf "0123456789."
    case readMaybe raw of
      Nothing -> fail "expected decimal number"
      Just dec -> pure $ ENumber dec

ref :: Parser Expr
ref = braces $ do
    i <- natural
    lexeme $ char ','
    j <- natural
    pure $ ERef $ Coords (fromInteger i) (fromInteger j)

binary name op = Infix (reservedOp name *> pure (EBinOp op)) AssocLeft
prefix name op = Prefix $ reservedOp name *> pure (EUnOp op)

expression :: Parser Expr
expression = buildExpressionParser table terms
  where
    table =
        [ [ prefix "-" Negate ]
        , [ binary "*" Times, binary "/" Div ]
        , [ binary "+" Plus, binary "-" Minus ]
        ]
    terms = parens expression
        <|> ref
        <|> number
        <?> "term"

parseExpression :: String -> Either ParseError Expr
parseExpression = parse (whiteSpace *> expression <* eof) ""

-- Evaluator

updateSheetState :: (Coords, String) -> SheetState
                 -> (SheetState, Either String (Map Coords CellResult))
updateSheetState (coords, expr) oldState = runSheet oldState (eval coords expr)

eval :: MonadSheet m => Coords -> String -> m ()
eval coords expr = do
    if expr == ""
        then do
            storeCellResult coords $ Right Empty
            updateDependencies coords []
        else case parseExpression expr of
            Left err -> do
                storeCellResult coords $ Left $ ParseError $ show err
                updateDependencies coords []
            Right parsedExpr -> do
                storeExpression coords parsedExpr
                updateDependencies coords $ getExprDeps parsedExpr
                valueLookup <- getValueLookup
                storeCellResult coords $ embedEvalResult $ evalCell valueLookup parsedExpr
    levels <- getLevels coords
    for_ levels $ \level -> do
        valueLookup' <- getValueLookup
        for level $ \coords' -> do
            expr' <- getExpr coords'
            storeCellResult coords' $ embedEvalResult $ evalCell valueLookup' expr'

evalCell :: (Coords -> Maybe Decimal) -> Expr -> EvalResult
evalCell valueLookup ex = case ex of
    ERef coords -> case valueLookup coords of
        Just val -> pure val
        Nothing -> Left $ RefNotFound coords
    EBinOp op left right -> do
        leftRes <- evalCell valueLookup left
        rightRes <- evalCell valueLookup right
        case op of
            Plus  -> pure $ leftRes + rightRes
            Minus -> pure $ leftRes - rightRes
            Times -> pure $ leftRes * rightRes
            Div -> do
                when (rightRes == 0) $ Left DivByZero
                pure $ leftRes / rightRes
    EUnOp op ex' -> case op of
        Negate -> do
          res <- evalCell valueLookup ex'
          pure $ -1 * res
    ENumber x -> pure x

-- Single cell of the spreadsheet

cell :: MonadWidget t m
     => Event t CellResult
     -> m (Event t String)
cell evalEv = el "div" $ do
    raw <- textInput def
    cellResult <- holdDyn (Right Empty) evalEv
    cellResultText <- mapDyn show cellResult
    dynText cellResultText
    pure $ _textInput_input raw

sheet :: MonadWidget t m
      => Map Coords CellResult
      -> Event t (Map Coords CellResult)
      -> m (Event t (Coords, String))
sheet list events = do
    dyn <- listWithKeyShallowDiff list (fmap Just <$> events) $ \_ _ e -> cell e
    dynEvent <- mapDyn (leftmost . map (\(k, e) -> (\ex -> (k, ex)) <$> e) . Map.toList) dyn
    pure $ switchPromptlyDyn dynEvent

cells :: MonadWidget t m => m ()
cells = el "div" $ do
  text "Reference other cells with {i,j}, for example top-left is {0,0}."
  let size = Size 1 10
      initial = Map.fromList
          [ (Coords i j, Right Empty) |
            i <- [0 .. (width size - 1)]
          , j <- [0 .. (height size - 1)]
          ]
  rec (_, eventMap) <- foldDynWithEvent updateSheetState (newSheetState size, Right initial) updates
      updates <- sheet initial (fmapMaybe (either (const Nothing) Just) eventMap)
  -- Errors
  dynError <- holdDyn "" $ fmapMaybe (either Just (const Nothing)) eventMap
  text "Error: "
  dynText dynError
  pure ()
