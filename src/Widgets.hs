{-# LANGUAGE RecursiveDo #-}

module Widgets where

import           Reflex
import           Reflex.Dom

import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeM)

import           Text.Read        (readMaybe)

import           Utils

readableInput :: (MonadWidget t m, Read a) => TextInputConfig t -> m (Event t a)
readableInput conf = do
    c <- textInput conf
    pure $ fmapMaybe readMaybe $ _textInput_input c

maybeButton :: MonadWidget t m
            => Dynamic t Bool
            -- ^ Is the button enabled?
            -> String
            -- ^ Static button label
            -> m (Event t ())
maybeButton enabled label = do
    attrs <- forDyn enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

datePicker :: MonadWidget t m
           => Dynamic t Bool
           -- ^ Widget enabled?
           -> m (Dynamic t (Maybe UTCTime))
datePicker enabled = do
    rec raw <- textInput $ def & textInputConfig_attributes .~ attrs
        date <- mapDyn (parseTimeM True defaultTimeLocale "%F") $ _textInput_value raw
        attrs <- dynCombine date enabled $ \d e ->
            monoidGuard (isNothing d) ("style" =: "color: red") <>
            monoidGuard (not e) ("disabled" =: "disabled")
    return date

selectableList :: (MonadWidget t m, Ord k)
               => Dynamic t (Maybe k)
               -- ^ Key of element that may be selected
               -> Dynamic t (Map k v)
               -- ^ Map of elements to be shown in the list
               -> (Dynamic t Bool -> Dynamic t v -> m (Event t a))
               -- ^ Action that renders a widget for an element. The element may fire events
               -> m (Event t k)
               -- ^ List fires events whenever an element is selected
selectableList selection elems mkEntry = do
    selectEntry <- listWithKey elems $ \k v -> do
        isSelected <- forDyn selection $ \s -> s == Just k
        fmap (const k) <$> mkEntry isSelected v
    switchPromptlyDyn <$> mapDyn (leftmost . Map.elems) selectEntry
