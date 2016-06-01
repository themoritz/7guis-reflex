module Main where

import           Reflex.Dom

import           Data.Time.Clock           (getCurrentTime)

import           GUIs.Cells
import           GUIs.CircleDrawer
import           GUIs.Counter
import           GUIs.CRUD
import           GUIs.FlightBooker
import           GUIs.TemperatureConverter
import           GUIs.Timer

main :: IO ()
main = do
    tStart <- getCurrentTime
    mainWidget $ do
        header "Counter"
        counter
        header "Temperature Converter"
        temperatureConverter
        header "Flight Booker"
        flightBooker
        header "Timer"
        timer tStart
        header "CRUD"
        crud
        header "Circle Drawer"
        circleDrawer
        header "Cells"
        cells

header :: MonadWidget t m => String -> m ()
header = el "h1" . text
