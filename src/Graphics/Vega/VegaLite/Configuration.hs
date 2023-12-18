{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Graphics.Vega.VegaLite.Configuration
  (
    -- * Helper Types
    TimeEncoding(..)
  , AxisBounds(..)
  , ViewConfig(..)
  , Width(..)
  , Height(..)
    -- * helpers
  , intYear
  , sizeOfContainerVC
  , fixedSizeVC
--  , timeField
  , configuredVegaLite
  , configuredVegaLiteSchema
  , viewConfigAsHvega
  , viewConfigAsTopLevel
    -- * Re-export
  )
where

import qualified Graphics.Vega.VegaLite        as GV
import qualified Graphics.Vega.VegaLite.Compat as Compat
import           Data.Text                      ( Text )

data AxisBounds a where
  Default ::AxisBounds a
  DataMinMax ::AxisBounds a
  GivenMinMax ::a -> a -> AxisBounds a deriving (Eq, Show)


-- even if set to container, we need a default
data Width = WidthContainer Double | WidthStep Double Double | WidthFixed Double deriving (Show)
defaultWidth :: Width -> Double
defaultWidth (WidthContainer x) = x
defaultWidth (WidthStep _ x) = x
defaultWidth (WidthFixed x) = x

data Height = HeightContainer Double | HeightFixed Double deriving (Show)
defaultHeight :: Height -> Double
defaultHeight (HeightContainer x) = x
defaultHeight (HeightFixed x) = x

topLevelViewWidth :: Width -> GV.PropertySpec
topLevelViewWidth (WidthContainer _) = GV.widthOfContainer
topLevelViewWidth (WidthStep x _) = GV.widthStep x
topLevelViewWidth (WidthFixed x) = GV.width x

topLevelViewHeight :: Height -> GV.PropertySpec
topLevelViewHeight (HeightContainer _) = GV.heightOfContainer
topLevelViewHeight (HeightFixed x) = GV.height x

data ViewConfig = ViewConfig { vcWidth :: Width, vcHeight :: Height, vcPadding :: Double }

sizeOfContainerVC :: Double -> Double -> Double -> ViewConfig
sizeOfContainerVC w h p = ViewConfig (WidthContainer w) (HeightContainer h) p

fixedSizeVC :: Double -> Double -> Double -> ViewConfig
fixedSizeVC w h p = ViewConfig (WidthFixed w) (HeightFixed h) p


viewConfigAsHvega :: ViewConfig -> GV.BuildConfigureSpecs
viewConfigAsHvega (ViewConfig w h p) = GV.configuration
  (GV.ViewStyle
    [ GV.ViewContinuousWidth $ defaultWidth w
    , GV.ViewContinuousHeight $ defaultHeight h
    , GV.ViewDiscreteWidth $ defaultWidth w
    , GV.ViewDiscreteHeight $ defaultHeight h
    ]
  )
viewConfigAsTopLevel :: ViewConfig -> [(GV.VLProperty, GV.VLSpec)]
viewConfigAsTopLevel (ViewConfig w h p) =
  [ topLevelViewWidth w
  , topLevelViewHeight h
  , GV.padding (GV.PSize p)
  , GV.autosize [GV.APad, GV.APadding, GV.AResize]
  ]

configuredVegaLite :: ViewConfig -> [(GV.VLProperty, GV.VLSpec)] -> GV.VegaLite
--configuredVegaLite vc xs =
--  GV.toVegaLite $ [(GV.configure . viewConfigAsHvega vc) []] <> xs
configuredVegaLite vc xs =
  GV.toVegaLite
    $  viewConfigAsTopLevel vc
    <> [(GV.configure . viewConfigAsHvega vc) []]
    <> xs


configuredVegaLiteSchema :: Text -> ViewConfig -> [(GV.VLProperty, GV.VLSpec)] -> GV.VegaLite
--configuredVegaLite vc xs =
--  GV.toVegaLite $ [(GV.configure . viewConfigAsHvega vc) []] <> xs
configuredVegaLiteSchema sch vc xs =
  GV.toVegaLiteSchema sch
    $  viewConfigAsTopLevel vc
    <> [(GV.configure . viewConfigAsHvega vc) []]
    <> xs


data TimeEncoding a = TimeEncoding { timeFormat :: Text, timeUnit :: Compat.BaseTimeUnitT, toDateTime:: a -> [GV.DateTime] }

-- helpers for time encoding
intYear :: TimeEncoding Int
intYear = TimeEncoding "%Y" GV.Year (\n -> [GV.DTYear n])

{-
timeField :: TimeEncoding a -> a -> GV.DataValue
timeField (TimeEncoding toStrF _ _) x = GV.Str $ toStrF x
-}
