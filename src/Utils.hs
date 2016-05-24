module Utils where

import           Reflex
import           Reflex.Dom

import           Data.Monoid
import           Data.Map (Map)

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
