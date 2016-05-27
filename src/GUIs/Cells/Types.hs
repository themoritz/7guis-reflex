module GUIs.Cells.Types where

data Size = Size
  { width :: Int
  , height :: Int
  } deriving (Show)

data Coords = Coords
  { i :: Int
  , j :: Int
  } deriving (Show, Eq, Ord)

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
    | ENumber Double
    | EEmpty
    deriving (Show)

getExprDeps :: Expr -> [Coords]
getExprDeps (ERef ref) = [ref]
getExprDeps (EBinOp _ l r) = getExprDeps l ++ getExprDeps r
getExprDeps (EUnOp _ sub) = getExprDeps sub
getExprDeps (ENumber _) = []
getExprDeps EEmpty = []

-- Eval

data EvalError
    = EvalRefNotFound Coords
    | EvalDivByZero
    | EvalEmpty
    deriving (Show)
