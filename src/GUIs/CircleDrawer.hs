module GUIs.CircleDrawer
    ( circleDrawer
    ) where

import           Reflex
import           Reflex.Dom

import           Data.Monoid
import           Data.Map    (Map)
import qualified Data.Map    as Map

import Utils


data Circle = Circle
    { circleX      :: Int
    , circleY      :: Int
    , circleRadius :: Int
    }

data Circles = Circles
    { circlesMap      :: Map Int Circle
    , circlesIndex    :: Int
    }

initialCircles :: Circles
initialCircles = Circles Map.empty 0

data CircleCommand
    = CirclePlace Circle
    | CircleAdjust Int Int   -- id radius

updateCircles :: CircleCommand -> Circles -> Circles
updateCircles cmd (Circles circles i) = case cmd of
    CirclePlace c      -> Circles (Map.insert i c circles) (i + 1)
    CircleAdjust sel r -> Circles (Map.update (\c -> Just $ c { circleRadius = r }) sel circles) i

data Stack a = Stack
    { stackStack     :: [a]
    , stackUndoSteps :: Int
    }

data StackCommand a
    = StackPush a
    | StackUndo
    | StackRedo

updateStack :: StackCommand a -> Stack a -> Stack a
updateStack cmd (Stack stack undo) = case cmd of
    StackPush x -> Stack (x:drop undo stack) 0
    StackUndo   -> Stack stack (undo + 1)
    StackRedo   -> Stack stack (undo - 1)

applyStack :: Stack CircleCommand -> Circles
applyStack (Stack stack undos) =
    foldr updateCircles initialCircles $ drop undos stack

circleDrawer :: MonadWidget t m => m ()
circleDrawer = el "div" $ do
    (svg, _) <- svgAttr' "svg" ("width" =: "100" <> "height" =: "100") $ do
        svgAttr "circle" ("cx" =: "50" <> "cy" =: "50" <> "r" =: "10") $ pure ()
        svgAttr "circle" ("cx" =: "20" <> "cy" =: "60" <> "r" =: "15") $ pure ()
    -- TODO: Get coordinates relative to SVG element
    coords <- holdDyn "" $ show <$> domEvent Mousemove svg
    dynText coords
    pure ()
