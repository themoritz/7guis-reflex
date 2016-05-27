{-# LANGUAGE RecursiveDo #-}

module GUIs.Cells
    ( cells
    ) where

import           Reflex
import           Reflex.Dom

import Control.Monad (when)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Control.Monad.State (State, evalState, get, gets, modify)
import Control.Monad.State.Class (MonadState)

import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Either
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Tree (Tree, Forest)
import qualified Data.Tree as Tree
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec hiding (State)
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
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
    pure $ ERef $ Coords (fromInteger i) (fromInteger j)

empty :: Parser Expr
empty = pure EEmpty

binary name op = Infix (reservedOp name *> pure (EBinOp op)) AssocLeft
prefix name op = Prefix $ reservedOp name *> pure (EUnOp op)

expr :: Parser Expr
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

type CellInput = Either EvalError Double

updateSheetState :: (Coords, Expr) -> SheetState -> (SheetState, Either String (Map Coords CellInput))
updateSheetState (coords, expr) oldState = runSheet oldState (eval coords expr)

eval :: MonadSheet m => Coords -> Expr -> m ()
eval coords expr = do
    storeExpression coords expr
    updateDependencies coords $ getExprDeps expr
    when hasCycles $ failEval "Cyclic references"
    valueLookup <- getValueLookup
    storeEvalResult coords (evalCell valueLookup expr)
    levels <- getLevels coords
    for_ levels $ \level -> do
        valueLookup' <- getValueLookup
        for level $ \coords' -> do
            expr' <- getExpr coords'
            storeEvalResult coords' (evalCell valueLookup' expr')

evalCell :: (Coords -> Maybe Double) -> Expr -> CellInput
evalCell valueLookup ex = case ex of
    ERef coords -> case valueLookup coords of
        Just val -> pure val
        Nothing -> Left $ EvalRefNotFound coords
    EBinOp op left right -> do
        leftRes <- evalCell valueLookup left
        rightRes <- evalCell valueLookup right
        case op of
            Plus -> pure $ leftRes + rightRes
            Minus -> pure $ leftRes - rightRes
            Times -> pure $ leftRes * rightRes
            Div -> do
                when (rightRes == 0) $ Left EvalDivByZero
                pure $ leftRes / rightRes
    EUnOp op ex' -> case op of
        Negate -> do
          res <- evalCell valueLookup ex'
          pure $ -1 * res
    ENumber x -> pure x
    EEmpty -> Left EvalEmpty

-- Single cell of the spreadsheet

data CellResult
    = CellErrorParse ParseError
    | CellErrorEval EvalError
    | CellResult Double
    | CellEmpty
    deriving (Show)

cell :: MonadWidget t m
     => Event t CellInput
     -> m (Event t Expr)
cell evalEv = el "div" $ do
    raw <- textInput def
    let eExpr = parseExpr <$> _textInput_input raw
        events = leftmost
          [ either CellErrorEval CellResult <$> evalEv
          , fmapMaybe (either (Just . CellErrorParse) (const Nothing)) eExpr
          ]
    cellResult <- holdDyn CellEmpty events
    cellResultText <- mapDyn show cellResult
    dynText cellResultText
    pure $ fmapMaybe (either (const Nothing) Just) eExpr

sheet :: MonadWidget t m
      => Map Coords CellInput
      -> Event t (Map Coords CellInput)
      -> m (Event t (Coords, Expr))
sheet list events = do
    dyn <- listWithKeyShallowDiff list (fmap Just <$> events) $ \c _ e -> el "div" $ do
        text $ show c
        cell e
    dynEvent <- mapDyn (leftmost . map (\(k, e) -> (\ex -> (k, ex)) <$> e) . Map.toList) dyn
    pure $ switchPromptlyDyn dynEvent

cells :: MonadWidget t m => m ()
cells = el "div" $ do
  let size = Size 2 3
      initial = Map.fromList
          [ (Coords i j, Right 0.0) |
            i <- [0 .. (width size - 1)]
          , j <- [0 .. (height size - 1)]
          ]
  rec (_, eventMap) <- foldDynWithEvent updateSheetState (newSheetState size, Right Map.empty) updates
      updates <- sheet initial (fmapMaybe (either (const Nothing) Just) eventMap)
  -- Errors
  dynError <- holdDyn "" $ fmapMaybe (either Just (const Nothing)) eventMap
  text "Error: "
  dynText dynError
  pure ()
