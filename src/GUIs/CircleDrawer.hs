{-# LANGUAGE RecursiveDo #-}

module GUIs.CircleDrawer
    ( circleDrawer
    ) where

import           Reflex
import           Reflex.Dom

import           Data.List (find)
import           Data.Monoid
import           Data.Map    (Map)
import qualified Data.Map    as Map

import GUIs.CircleDrawer.Stack

import Utils
import Widgets

data Circle = Circle
    { circleX      :: Int
    , circleY      :: Int
    , circleRadius :: Int
    }

data Circles = Circles
    { circlesMap      :: Map Int Circle
    , circlesIndex    :: Int
    , circlesSelected :: Int
    }

initialCircles :: Circles
initialCircles = Circles Map.empty 0 0

selected :: Circles -> Maybe Int
selected (Circles circles _ sel) = sel <$ Map.lookup sel circles

trySelect :: (Int, Int) -> Circles -> Maybe Int
trySelect (x, y) (Circles circles _ _) =
    fst <$> find withinRadius (reverse $ Map.toList circles)
  where
    withinRadius (_, Circle cx cy r) =
        let dx = x - cx
            dy = y - cy
        in  dx * dx + dy * dy < r * r

data CircleCommand
    = CirclePlace Circle
    | CircleSelect Int
    | CircleAdjust Int Int   -- id radius

updateCircles :: CircleCommand -> Circles -> Circles
updateCircles cmd (Circles circles i sel) = case cmd of
    CirclePlace c      -> Circles (Map.insert i c circles) (i + 1) sel
    CircleSelect s     -> Circles circles i s
    CircleAdjust sel r -> Circles (Map.update (\c -> Just $ c { circleRadius = r }) sel circles) i sel

circle :: MonadWidget t m
       => Dynamic t Bool -> Dynamic t Circle
       -> m (Event t ())
circle selected circle = do
    attr <- dynCombine selected circle $ \s (Circle x y r) ->
        ( "cx" =: show x <> "cy" =: show y <> "r" =: show r
       <> "fill" =: (if s then "gray" else "white")
       <> "stroke" =: "black"
        )
    (svg, _) <- svgDynAttr' "circle" attr $ pure ()
    pure $ () <$ domEvent (Mouseup RelativeToOffset) svg

circleDrawer :: MonadWidget t m => m ()
circleDrawer = el "div" $ mdo
    stack <- foldDyn updateStack initialStack commands
    state <- mapDyn (foldStack initialCircles updateCircles) stack
    selectedCircle <- mapDyn selected state
    circles <- mapDyn circlesMap state
    enableUndo <- mapDyn undoPossible stack
    enableRedo <- mapDyn redoPossible stack
    undo <- maybeButton enableUndo "Undo"
    redo <- maybeButton enableRedo "Redo"
    text "Radius:"
    changeRadius <- readableInput def
    el "br" $ pure ()
    (svg, _) <- svgAttr' "svg" ("width" =: "600" <> "height" =: "300") $ do
        selectableList selectedCircle circles circle
        svgAttr "rect" ("width" =: "600" <> "height" =: "300" <> "stroke" =: "black" <> "fill" =: "none") $ pure ()
    let svgClick = domEvent (Mouseup RelativeToOffset) svg
        svgEvent = attachWith (\st (x,y) -> StackPush $ case trySelect (x,y) st of
                Nothing -> CirclePlace $ Circle x y 50
                Just s -> CircleSelect s
            ) (current state) svgClick
        commands = leftmost
            [ svgEvent
            , fmapMaybe (\(ms, r) -> (\s -> StackPush $ CircleAdjust s r) <$> ms) $
                  attach (current selectedCircle) changeRadius
            , StackUndo <$ undo
            , StackRedo <$ redo
            ]
    pure ()
