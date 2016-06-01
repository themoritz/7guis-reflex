{-# LANGUAGE RecursiveDo #-}

module GUIs.CRUD
    ( crud
    ) where

import           Reflex
import           Reflex.Dom

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe
import           Data.Monoid

import           Utils
import           Widgets

data Person = Person String String -- name surname

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
