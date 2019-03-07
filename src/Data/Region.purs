module Data.Region where

import Extra.Prelude

data Region = Kitchen | Cave

derive instance eqRegion :: Eq Region
