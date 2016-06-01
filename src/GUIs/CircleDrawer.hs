{-# LANGUAGE RecursiveDo #-}

module GUIs.CircleDrawer
    ( circleDrawer
    ) where

import           Reflex
import           Reflex.Dom

import           Data.List               (find)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Monoid

import           GUIs.CircleDrawer.Stack

import           Utils
import           Widgets

data Circle = Circle Int Int Int -- x y radius

data State = State (Map Int Circle) Int Int -- map currentIndex selected

circlesMap :: State -> Map Int Circle
circlesMap (State m _ _) = m

initialState :: State
initialState = State Map.empty 0 0

getSelected :: State -> Maybe Int
getSelected (State circles _ sel) = sel <$ Map.lookup sel circles

trySelect :: (Int, Int) -> State -> Maybe Int
trySelect (x, y) (State circles _ _) =
    fst <$> find withinRadius (reverse $ Map.toList circles)
  where
    withinRadius (_, Circle cx cy r) =
        let dx = x - cx
            dy = y - cy
        in  dx * dx + dy * dy < r * r

data Command
    = CirclePlace Circle
    | CircleSelect Int
    | CircleAdjust Int Int   -- id radius

updateState :: Command -> State -> State
updateState cmd (State circles i sel) = case cmd of
    CirclePlace c      -> State (Map.insert i c circles) (i + 1) sel
    CircleSelect s     -> State circles i s
    CircleAdjust s r -> State (Map.update (\(Circle x y _) -> Just $ Circle x y r) s circles) i sel

circle :: MonadWidget t m
       => Dynamic t Bool -> Dynamic t Circle
       -> m (Event t ())
circle selected circleDyn = do
    attr <- dynCombine selected circleDyn $ \s (Circle x y r) ->
        ( "cx" =: show x <> "cy" =: show y <> "r" =: show r
       <> "fill" =: (if s then "gray" else "white")
       <> "stroke" =: "black"
        )
    (svg, _) <- svgDynAttr' "circle" attr $ pure ()
    pure $ () <$ domEvent (Mouseup RelativeToOffset) svg

circleDrawer :: MonadWidget t m => m ()
circleDrawer = el "div" $ mdo
    stack <- foldDyn updateStack initialStack commands
    state <- mapDyn (foldStack initialState updateState) stack
    selectedCircle <- mapDyn getSelected state
    circles <- mapDyn circlesMap state
    enableUndo <- mapDyn undoPossible stack
    enableRedo <- mapDyn redoPossible stack
    undo <- maybeButton enableUndo "Undo"
    redo <- maybeButton enableRedo "Redo"
    text "Radius:"
    changeRadius <- readableInput def
    el "br" $ pure ()
    (svg, _) <- svgAttr' "svg" ("width" =: "600" <> "height" =: "300") $ do
        _ <- selectableList selectedCircle circles circle
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
