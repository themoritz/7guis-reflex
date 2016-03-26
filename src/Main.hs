{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main where

import Reflex
import Reflex.Dom

import Data.Monoid
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

import Safe (readMay)

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

--performance--------------------------

perf :: MonadWidget t m => m ()
perf = do
  val <- textInput def
  let x = fmapMaybe (readMay :: String -> Maybe Int) $ _textInput_input val
  dynMap <- foldDyn (\n _ -> Map.fromList $ zip [1..n] [1..n]) Map.empty x
  el "table" $ list dynMap (const $ el "tr" $ list dynMap (const $ el "td" counter))
  pure ()

--widgets------------------------------

doubleInput :: MonadWidget t m => TextInputConfig t -> m (Event t Double)
doubleInput conf = do
    c <- textInput conf
    pure $ fmapMaybe readMay $ _textInput_input c

maybeButton :: MonadWidget t m => Dynamic t Bool -> String -> m (Event t ())
maybeButton enabled label = do
  attrs <- mapDyn (\e -> if e
      then mempty
      else "disabled" =: "disabled"
    ) enabled
  (btn, _) <- elDynAttr' "button" attrs $ text label
  pure $ domEvent Click btn

selectableList :: (MonadWidget t m, Ord k)
           => Dynamic t (Maybe k) -> Dynamic t (Map k v)
           -> (Dynamic t v -> Dynamic t Bool -> m (Event t a))
           -> m (Event t k)
selectableList selection elems mkEntry = do
  selectEntry <- listWithKey elems $ \k v -> do
    isSelected <- forDyn selection $ \s -> s == Just k
    fmap (const k) <$> mkEntry v isSelected
  switchPromptlyDyn <$> mapDyn (leftmost . Map.elems) selectEntry

header :: MonadWidget t m => String -> m ()
header = el "h3" . text

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

  cNum <- doubleInput $ def & textInputConfig_setValue .~ cStr
  let fComp = (\x -> x * 9/5 + 32) <$> cNum
      fStr = show <$> fComp

  text "Celsius = "

  fNum <- doubleInput $ def & textInputConfig_setValue .~ fStr
  let cComp = (\x -> x * 5/9 - 32) <$> fNum
      cStr = show <$> cComp

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
  bookEnabled <- mapDyn isJust bookMsg
  bookBtn <- maybeButton bookEnabled "Book!"

  resp <- holdDyn Nothing $ tagDyn bookMsg bookBtn
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

--4------------------------------------

-- | TODO: Stop timer when limit is reached
timer :: MonadWidget t m => UTCTime -> m ()
timer t0 = el "div" $ mdo
  current <- fmap _tickInfo_lastUTC <$> tickLossy 0.1 t0
  currentDyn <- holdDyn t0 current

  limit <- doubleInput def
  limitDyn <- holdDyn 10.0 limit
  elapsed <- combineDyn3 (\c s l ->
      show $ min (diffUTCTime c s) (realToFrac l)
    ) currentDyn startDyn limitDyn

  dynText elapsed

  start <- tagDyn currentDyn <$> button "Reset"
  startDyn <- holdDyn t0 start
  pure ()

--5------------------------------------

data Person = Person
  { personName :: String
  , personSurname :: String
  }

instance Show Person where
  show (Person name surname) = surname <> ", " <> name

data DB = DB
  { dbPersons :: Map Int Person
  , dbSelected :: Int
  , dbIndex :: Int
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
selected (DB persons sel _) =
  if Map.member sel persons
    then Just sel
    else Nothing

crud :: MonadWidget t m => m ()
crud = el "div" $ mdo
  name <- textInput def
  surname <- textInput def
  person <- combineDyn Person (_textInput_value name) (_textInput_value surname)

  db <- foldDyn updateDB initialDB updates
  persons <- mapDyn dbPersons db
  personSelected <- mapDyn selected db
  isPersonSelected <- mapDyn isJust personSelected

  select <- el "ul" $ selectableList personSelected persons $ \p s -> do
    attrs <- mapDyn (\s' -> "style" =: if s' then "color: blue" else "") s
    domEvent Click . fst <$> elDynAttr' "li" attrs (display p)

  create <- button "Create"
  update <- maybeButton isPersonSelected "Update"
  delete <- maybeButton isPersonSelected "Delete"

  personToUpdate <- combineDyn (,) personSelected person
  let updates = leftmost
        [ DBDelete <$> fmapMaybe id (tag (current personSelected) delete)
        , fmapMaybe (\(mSel, p) -> case mSel of
              Nothing -> Nothing
              Just sel -> Just $ DBUpdate sel p
            ) $ tag (current personToUpdate) update
        , DBInsert <$> tag (current person) create
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
  text "Circle"

--utils--------------------------------

combineDyn3 :: (Reflex t, MonadHold t m)
            => (a -> b -> c -> d)
            -> Dynamic t a -> Dynamic t b -> Dynamic t c
            -> m (Dynamic t d)
combineDyn3 f da db dc = do
  dg <- combineDyn f da db
  combineDyn (\g c -> g c) dg dc
