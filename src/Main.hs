{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase  #-}

module Main where

import           Reflex
import           Reflex.Dom

import           Data.Decimal
import           Data.Monoid
import           Data.Maybe
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Time.Clock  (UTCTime, getCurrentTime, diffUTCTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)

import           Text.Read        (readMaybe)

main :: IO ()
main = do
    tStart <- getCurrentTime
    mainWidget $ do
        header "Counter"
        counter
        header "Temperature Converter"
        temperature
        header "Flight Booker"
        flight
        header "Timer"
        timer tStart
        header "CRUD"
        crud
        header "Circle Drawer"
        circle
        header "Cells"
        cells

--widgets------------------------------

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

header :: MonadWidget t m => String -> m ()
header = el "h1" . text

--1------------------------------------

counter :: MonadWidget t m => m ()
counter = el "div" $ do

    click <- button "Click"
    c <- count click
    cStr <- mapDyn (show :: Int -> String) c

    text "Clicks: "
    dynText cStr

    pure ()

--2------------------------------------

temperature :: MonadWidget t m => m ()
temperature = el "div" $ mdo
    celsius <- readableInput $ def & textInputConfig_setValue
        .~ ((\x -> show $ ((x :: Double) - 32) * 5/9) <$> fahrenheit)
    text "Celsius = "

    fahrenheit <- readableInput $ def & textInputConfig_setValue
        .~ ((\x -> show $ (x :: Double) * 9/5 + 32) <$> celsius)
    text "Fahrenheit"

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
flight = el "div" $ do

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

--4------------------------------------

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

--5------------------------------------

data Person = Person
    { personName    :: String
    , personSurname :: String
    }

instance Show Person where
    show (Person name surname) = surname <> ", " <> name

data DB = DB
    { dbPersons  :: Map Int Person
    , dbSelected :: Int
    , dbIndex    :: Int
    } deriving (Show)

initialDB :: DB
initialDB = DB Map.empty 0 0

data DBCommand
    = DBInsert Person
    | DBUpdate Int Person
    | DBDelete Int
    | DBSelect Int

updateDB :: DBCommand -> DB -> DB
updateDB cmd (DB persons sel ind) = case cmd of
    DBInsert p   -> DB (Map.insert ind p persons) sel (ind + 1)
    DBUpdate i p -> DB (Map.update (const $ Just p) i persons) sel ind
    DBDelete i   -> DB (Map.delete i persons) sel ind
    DBSelect i   -> DB persons i ind

selected :: DB -> Maybe Int
selected (DB persons sel _) = if Map.member sel persons
    then Just sel
    else Nothing

crud :: MonadWidget t m => m ()
crud = el "div" $ do
    text "Name:"
    name <- textInput def
    text "Surname:"
    surname <- textInput def
    person <- combineDyn Person (_textInput_value name) (_textInput_value surname)

    rec db <- foldDyn updateDB initialDB updates
        persons <- mapDyn dbPersons db
        selectedPerson <- mapDyn selected db
        isPersonSelected <- mapDyn isJust selectedPerson

        select <- el "ul" $ selectableList selectedPerson persons $ \sel p -> do
          attrs <- mapDyn (\s -> monoidGuard s $ "style" =: "font-weight: bold") sel
          domEvent Click . fst <$> elDynAttr' "li" attrs (display p)

        createClick <- button "Create"
        updateClick <- maybeButton isPersonSelected "Update"
        deleteClick <- maybeButton isPersonSelected "Delete"

        personToUpdate <- combineDyn (,) selectedPerson person

        let updates = leftmost
              [ DBDelete <$> fmapMaybe id (tag (current selectedPerson) deleteClick)
              , fmapMaybe (\(mSel, p) -> case mSel of
                    Nothing -> Nothing
                    Just sel -> Just $ DBUpdate sel p
                  ) $ tag (current personToUpdate) updateClick
              , DBInsert <$> tag (current person) createClick
              , DBSelect <$> select
              ]

    pure ()

--6------------------------------------

data Circle = Circle
    { circleX      :: Int
    , circleY      :: Int
    , circleRadius :: Int
    }

data Circles = Circles
    { circlesMap      :: Map Int Circle
    , circlesIndex    :: Int
    }

initialCircles :: Circles
initialCircles = Circles Map.empty 0

data CircleCommand
    = CirclePlace Circle
    | CircleAdjust Int Int   -- id radius

updateCircles :: CircleCommand -> Circles -> Circles
updateCircles cmd (Circles circles i) = case cmd of
    CirclePlace c      -> Circles (Map.insert i c circles) (i + 1)
    CircleAdjust sel r -> Circles (Map.update (\c -> Just $ c { circleRadius = r }) sel circles) i

data Stack a = Stack
    { stackStack     :: [a]
    , stackUndoSteps :: Int
    }

data StackCommand a
    = StackPush a
    | StackUndo
    | StackRedo

updateStack :: StackCommand a -> Stack a -> Stack a
updateStack cmd (Stack stack undo) = case cmd of
    StackPush x -> Stack (x:drop undo stack) 0
    StackUndo   -> Stack stack (undo + 1)
    StackRedo   -> Stack stack (undo - 1)

applyStack :: Stack CircleCommand -> Circles
applyStack (Stack stack undos) =
    foldr updateCircles initialCircles $ drop undos stack

circle :: MonadWidget t m => m ()
circle = el "div" $ do
    (svg, _) <- svgAttr' "svg" ("width" =: "100" <> "height" =: "100") $ do
        svgAttr "circle" ("cx" =: "50" <> "cy" =: "50" <> "r" =: "10") $ pure ()
        svgAttr "circle" ("cx" =: "20" <> "cy" =: "60" <> "r" =: "15") $ pure ()
    -- TODO: Get coordinates relative to SVG element
    coords <- holdDyn "" $ show <$> domEvent Mousemove svg
    dynText coords
    pure ()

--7------------------------------------

cells :: MonadWidget t m => m ()
cells = el "div" $ pure ()

--utils--------------------------------

svgAttr' :: MonadWidget t m => String -> Map String String -> m a -> m (El t, a)
svgAttr' name attrs = elDynAttrNS' (Just "http://www.w3.org/2000/svg") name (constDyn attrs)

svgAttr :: MonadWidget t m => String -> Map String String -> m a -> m a
svgAttr name attrs childs = snd <$> svgAttr' name attrs childs

dynCombine :: (Reflex t, MonadHold t m)
           => Dynamic t a -> Dynamic t b
           -> (a -> b -> c)
           -> m (Dynamic t c)
dynCombine a b f = combineDyn f a b

dynCombine3 :: (Reflex t, MonadHold t m)
            => Dynamic t a -> Dynamic t b -> Dynamic t c
            -> (a -> b -> c -> d)
            -> m (Dynamic t d)
dynCombine3 da db dc f = do
  dg <- combineDyn f da db
  combineDyn (\g c -> g c) dg dc

monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty
