module Data.Region where

import Extra.Prelude

data Region = Kitchen | Cave

derive instance eqRegion :: Eq Region
derive instance ordRegion :: Ord Region

regions :: Array Region
regions = [Kitchen, Cave]
