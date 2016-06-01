module GUIs.Cells.Types where

import           Data.Decimal
import           Data.Monoid

data Size = Size
  { width  :: Int
  , height :: Int
  } deriving (Show)

data Coords
  = Coords Int Int -- col row
  deriving (Show, Eq, Ord)

inBounds :: Size -> Coords -> Bool
inBounds (Size w h) (Coords i j) = i < w && j < h && i >= 0 && j >= 0

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
    = ERef Coords
    | EBinOp BinOp Expr Expr
    | EUnOp UnOp Expr
    | ENumber Decimal
    deriving (Show)

getExprDeps :: Expr -> [Coords]
getExprDeps (ERef ref) = [ref]
getExprDeps (EBinOp _ l r) = getExprDeps l ++ getExprDeps r
getExprDeps (EUnOp _ sub) = getExprDeps sub
getExprDeps (ENumber _) = []

-- Cell result

type CellResult = Either CellError CellOk

data CellOk
    = Number Decimal
    | Empty
    deriving (Show)

data CellError
    = EvalError EvalError
    | ParseError String
    deriving (Show)

type EvalResult = Either EvalError Decimal

data EvalError
    = RefNotFound Coords
    | DivByZero
    deriving (Show)

embedEvalResult :: EvalResult -> CellResult
embedEvalResult (Left err) = Left $ EvalError err
embedEvalResult (Right x) = Right $ Number x

showCellResult :: CellResult -> String
showCellResult (Left (EvalError (RefNotFound (Coords i j)))) = "{" <> show i <> "," <> show j <> "} not defined"
showCellResult (Left (EvalError DivByZero)) = "Division by zero"
showCellResult (Left (ParseError _)) = "Cannot parse expression"
showCellResult (Right (Number x)) = "=" <> show x
showCellResult (Right Empty) = "(empty)"
