{-# LANGUAGE RecursiveDo #-}

module GUIs.Timer
    ( timer
    ) where

import           Reflex
import           Reflex.Dom

import           Data.Decimal
import           Data.Time.Clock  (UTCTime, getCurrentTime, diffUTCTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)

import Widgets


data TimerEvent
  = TimerTick Decimal -- limit
  | TimerReset

timer :: MonadWidget t m => UTCTime -> m ()
timer t0 = el "div" $ do
    tick <- tickLossy 0.1 t0

    text "Limit:"
    limit <- readableInput def
    limitDyn <- holdDyn 10.0 limit

    rec let events = leftmost
              [ (\(limit', _) -> TimerTick limit') <$> attachDyn limitDyn tick
              , const TimerReset <$> reset
              ]

        elapsed <- foldDyn (\ev current -> case ev of
                TimerTick limit' -> if current + 0.1 <= limit' then current + 0.1 else current
                TimerReset       -> 0.0
            ) (0.0 :: Decimal) events

        elapsedText <- mapDyn show elapsed
        dynText elapsedText

        reset <- button "Reset"
    pure ()
