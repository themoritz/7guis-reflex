{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GUIs.Cells.Sheet
    ( SheetState
    , newSheetState
    , MonadSheet(..)
    , runSheet
    ) where

import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Control.Monad.State (State, evalState, get, gets, modify)
import Control.Monad.State.Class (MonadState)

import Data.Array ((//))
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Tree (Tree, Forest)
import qualified Data.Tree as Tree
import Data.Map (Map)
import qualified Data.Map as Map

import GUIs.Cells.Types

data SheetState = SheetState
    { ssSize :: Size
    , ssDependencies :: Graph
    , ssValues :: Map Coords Double
    , ssExpressions :: Map Coords Expr
    , ssUpdates :: Map Coords (Either EvalError Double)
    }

newSheetState :: Size -> SheetState
newSheetState size = SheetState
    { ssSize = size
    , ssDependencies = emptyGraph size
    , ssValues = Map.empty
    , ssExpressions = Map.empty
    , ssUpdates = Map.empty
    }

class Monad m => MonadSheet m where
    failEval :: String -> m a
    storeEvalResult :: Coords -> Either EvalError Double -> m ()
    storeExpression :: Coords -> Expr -> m ()
    getValueLookup :: m (Coords -> Maybe Double)
    getExpr :: Coords -> m Expr
    updateDependencies :: Coords -> [Coords] -> m ()
    hasCycles :: m Bool
    getLevels :: Coords -> m [[Coords]]

newtype Sheet a = Sheet
    { unSheet :: EitherT String (State SheetState) a
    } deriving (Functor, Applicative, Monad, MonadState SheetState)

runSheet :: SheetState ->  Sheet a -> (SheetState, Either String (Map Coords (Either EvalError Double)))
runSheet old actions = flip evalState old $ do
    failed <- runEitherT (unSheet actions)
    state <- get
    let cleanedState = state
            { ssUpdates = Map.empty
            }
    case failed of
        Left err -> pure (cleanedState, Left err)
        Right _ -> pure (cleanedState, Right $ ssUpdates state)

instance MonadSheet Sheet where
    failEval err = Sheet $ left err
    storeEvalResult coords result = modify $ \st -> st
        { ssValues = case result of
              Left _ -> ssValues st
              Right x -> Map.insert coords x (ssValues st)
        , ssUpdates = Map.insert coords result (ssUpdates st)
        }
    storeExpression coords expr = modify $ \st -> st
        { ssExpressions = Map.insert coords expr (ssExpressions st)
        }
    getValueLookup = do
        valueMap <- gets ssValues
        pure $ \c -> Map.lookup c valueMap
    getExpr coords = do
        exprMap <- gets ssExpressions
        case Map.lookup coords exprMap of
            Just expr -> pure expr
            Nothing -> failEval $ "Could not find expression for " ++ show coords
    updateDependencies coords deps = do
        size <- gets ssSize
        let vertex = toVertex size
        modify $ \st -> st
            { ssDependencies = ssDependencies st // [(vertex coords, map vertex deps)]
            }
    hasCycles = do
        deps <- gets ssDependencies
        size <- gets ssSize
        let sccs = map (fmap (fromVertex size)) $ Graph.scc deps
        pure . not . null $ filter (\t -> length (Tree.flatten t) > 1) sccs
    getLevels coords = do
        deps <- gets ssDependencies
        size <- gets ssSize
        case Graph.dfs (Graph.transposeG deps) [toVertex size coords] of
            [tree] -> pure $ Tree.levels $ fromVertex size <$> tree
            _ -> failEval "Expected one forest for levels"
