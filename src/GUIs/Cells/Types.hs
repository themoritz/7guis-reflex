module GUIs.Cells.Types where

import Data.Graph (Graph, buildG, Vertex)

data Size = Size
  { width :: Int
  , height :: Int
  }

data Coords = Coords
  { i :: Int
  , j :: Int
  } deriving (Show, Eq, Ord)

toVertex :: Size -> Coords -> Vertex
toVertex (Size w _) (Coords i j) = w * j + i

fromVertex :: Size -> Vertex -> Coords
fromVertex (Size w _) x = Coords (x `div` w) (x `mod` w)

emptyGraph :: Size -> Graph
emptyGraph (Size w h) = buildG (0, w * h - 1) []

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
