{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

module Main where

import Reflex
import Reflex.Dom

import Data.Monoid
import Data.Maybe (isJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

import Safe (readMay)

main :: IO ()
main = mainWidget $ do
  counter
  el "hr" $ pure ()
  temperature
  el "hr" $ pure ()
  flight

--1------------------------------------

counter :: MonadWidget t m => m ()
counter = el "div" $ mdo

  dynText cStr

  click <- button "Click"
  (c :: Dynamic t Int) <- count click
  cStr <- mapDyn show c

  pure ()

--2------------------------------------

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

  where
    mReadDouble :: String -> Maybe Double
    mReadDouble = readMay

--3------------------------------------

data FlightType
  = OneWay
  | Return
  deriving (Eq, Ord, Read, Show)

flightTypeMap :: Map FlightType String
flightTypeMap = Map.fromList
  [ (OneWay, "one-way flight")
  , (Return, "return flight")
  ]

flight :: MonadWidget t m => m ()
flight = el "div" $ mdo

  flightType <- dropdown OneWay (constDyn flightTypeMap) def

  mStart <- datePicker (constDyn True)

  endEnabled <- mapDyn (\case OneWay -> False
                              Return -> True) $ _dropdown_value flightType
  mEnd <- datePicker endEnabled

  bookMsg <- combineDyn3 (\ft ms me -> case (ft, ms, me) of
      (OneWay, Just s, _) ->
        Just $ "You have booked a one-way flight on " <> show s <> "."
      (Return, Just s, Just e) ->
        if s < e
          then Just $ "You have booked a return trip from " <> show s <> " to " <> show s <> "."
          else Nothing
      _ ->
        Nothing
    ) (_dropdown_value flightType) mStart mEnd
  bookAttrs <- mapDyn (\msg ->
      if isJust msg
        then mempty
        else "disabled" =: "disabled"
    ) bookMsg
  (bookBtn, _) <- elDynAttr' "button" bookAttrs $ text "Book!"

  resp <- holdDyn Nothing $ tagDyn bookMsg (domEvent Click bookBtn)
  respStr <- mapDyn show resp

  dynText respStr

  pure ()

  where
    datePicker :: MonadWidget t m
               => Dynamic t Bool
               -> m (Dynamic t (Maybe UTCTime))
    datePicker enabled = mdo
      raw <- textInput $ def & textInputConfig_attributes .~ attrs
      date <- mapDyn (parseTimeM True defaultTimeLocale "%F") $
                _textInput_value raw
      attrs <- combineDyn (\d e ->
          "style" =: (if isJust d then "" else "color: red")
          <> if e then mempty else "disabled" =: "disabled"
        ) date enabled
      pure date

--utils--------------------------------

combineDyn3 :: (Reflex t, MonadHold t m)
            => (a -> b -> c -> d)
            -> Dynamic t a -> Dynamic t b -> Dynamic t c
            -> m (Dynamic t d)
combineDyn3 f da db dc = do
  dg <- combineDyn f da db
  combineDyn (\g c -> g c) dg dc
