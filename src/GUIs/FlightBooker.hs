{-# LANGUAGE LambdaCase #-}

module GUIs.FlightBooker
    ( flightBooker
    ) where

import           Reflex
import           Reflex.Dom

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe
import           Data.Monoid

import           Utils
import           Widgets

data FlightType
    = OneWay
    | Return
    deriving (Eq, Ord, Read, Show)

flightTypeMap :: Map FlightType String
flightTypeMap = Map.fromList
    [ (OneWay, "one-way flight")
    , (Return, "return flight")
    ]

flightBooker :: MonadWidget t m => m ()
flightBooker = el "div" $ do

    flightType <- dropdown OneWay (constDyn flightTypeMap) def

    text "Depart:"
    mStart <- datePicker (constDyn True)

    endEnabled <- forDyn (_dropdown_value flightType) $ \case
        OneWay -> False
        Return -> True
    text "Return:"
    mEnd <- datePicker endEnabled

    bookMsg <- dynCombine3 (_dropdown_value flightType) mStart mEnd $ \ft ms me -> case (ft, ms, me) of
        (OneWay, Just s, _) ->
            Just $ "You have booked a one-way flight on " <> show s <> "."
        (Return, Just s, Just e) -> if s < e
            then Just $ "You have booked a return trip from " <> show s <> " to " <> show s <> "."
            else Nothing
        _ ->
            Nothing
    bookEnabled <- mapDyn isJust bookMsg
    bookBtn <- maybeButton bookEnabled "Book!"

    resp <- holdDyn Nothing $ tagDyn bookMsg bookBtn
    respStr <- mapDyn show resp

    dynText respStr
