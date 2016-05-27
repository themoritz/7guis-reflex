{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GUIs.Cells.Sheet
    ( SheetState
    , newSheetState
    , MonadSheet(..)
    , runSheet
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Control.Monad.State (State, evalState, get, gets, modify)
import Control.Monad.State.Class (MonadState)

import Data.Map (Map)
import qualified Data.Map as Map

import GUIs.Cells.Types
import GUIs.Cells.References

data SheetState = SheetState
    { ssSize :: Size
    , ssDependencies :: ReferenceGraph Coords
    , ssValues :: Map Coords Double
    , ssExpressions :: Map Coords Expr
    , ssUpdates :: Map Coords (Either EvalError Double)
    } deriving (Show)

newSheetState :: Size -> SheetState
newSheetState size@(Size w h) = SheetState
    { ssSize = size
    , ssDependencies = mkEmpty (w * h - 1)
                               (\(Coords i j) -> i * w + j)
                               (\v -> Coords (v `div` w) (v `mod` w))
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
        when (coords `elem` deps) $ failEval "Cannot reference self."
        depsGraph <- gets ssDependencies
        when (hasCycle depsGraph) $ failEval "Cyclic references"
        let depsGraph' = addReferences coords (filter (inbounds size) deps) depsGraph
        modify $ \st -> st { ssDependencies = depsGraph'}
    getLevels coords = do
        deps <- gets ssDependencies
        maybe (failEval "Expected one forest for levels") pure $ levelsAtVertex deps coords
