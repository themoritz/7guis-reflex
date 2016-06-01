module Utils where

import           Reflex
import           Reflex.Dom

import           Control.Monad.Fix

import           Data.Map          (Map)

svgAttr' :: MonadWidget t m => String -> Map String String -> m a -> m (El t, a)
svgAttr' name attrs = svgDynAttr' name (constDyn attrs)

svgDynAttr' :: MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m (El t, a)
svgDynAttr' = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

svgDynAttr :: MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m a
svgDynAttr name attrs childs = snd <$> svgDynAttr' name attrs childs

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

foldDynWithEvent :: (Reflex t, MonadHold t m, MonadFix m)
                 => (a -> b -> (b, c))
                 -> (b, c)
                 -> Event t a
                 -> m (Dynamic t b, Event t c)
foldDynWithEvent f start ev = do
    dynbc <- foldDyn (\a (b, _) -> f a b) start ev
    dynb <- mapDyn fst dynbc
    pure (dynb, snd <$> updated dynbc)
