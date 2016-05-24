module GUIs.Cells
    ( cells
    ) where

import           Reflex
import           Reflex.Dom

cells :: MonadWidget t m => m ()
cells = el "div" $ pure ()
