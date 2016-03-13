{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  el "hr" $ pure ()
  counter
  el "hr" $ pure ()

counter :: MonadWidget t m => m ()
counter = el "div" $ mdo

  dynText cStr

  click <- button "Click"
  (c :: Dynamic t Int) <- count click
  cStr <- mapDyn show c

  pure ()
