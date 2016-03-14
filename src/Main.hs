{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reflex
import Reflex.Dom

import Safe (readMay)

main :: IO ()
main = mainWidget $ do
  counter
  el "hr" $ pure ()
  temperature

counter :: MonadWidget t m => m ()
counter = el "div" $ mdo

  dynText cStr

  click <- button "Click"
  (c :: Dynamic t Int) <- count click
  cStr <- mapDyn show c

  pure ()

temperature :: MonadWidget t m => m ()
temperature = el "div" $ mdo

  c <- textInput $ def & textInputConfig_setValue .~ cStr
  let cNum = fmapMaybe mReadDouble $ _textInput_input c
      fComp = (\x -> x * 9/5 + 32) <$> cNum
      fStr = show <$> fComp

  text "Celsius = "

  f <- textInput $ def & textInputConfig_setValue .~ fStr
  let fNum = fmapMaybe mReadDouble $ _textInput_input f
      cComp = (\x -> x * 5/9 - 32) <$> fNum
      cStr = show <$> cComp

  text "Fahrenheit"

  where mReadDouble :: String -> Maybe Double
        mReadDouble = readMay
