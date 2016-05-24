module GUIs.Counter
    ( counter
    ) where

import           Reflex
import           Reflex.Dom

counter :: MonadWidget t m => m ()
counter = el "div" $ do

  click <- button "Click"
  c <- count click
  cStr <- mapDyn (show :: Int -> String) c

  text "Clicks: "
  dynText cStr

  pure ()
