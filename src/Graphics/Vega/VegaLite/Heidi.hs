{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Vega.VegaLite.Heidi where
import qualified Data.Scientific as Scientific
import qualified Graphics.Vega.VegaLite        as GV
import qualified Heidi
import Heidi (VP(..))

-- the goal
-- get names from the TC and values from the VP
vpTodv :: VP -> GV.DataValue
vpTodv = \case
  VPInt x -> GV.Number $ realToFrac x
  VPInt8 x -> GV.Number $ realToFrac x
  VPInt16 x -> GV.Number $ realToFrac x
  VPInt32 x -> GV.Number $ realToFrac x
  VPInt64 x -> GV.Number $ realToFrac x
  VPWord x -> GV.Number $ realToFrac x
  VPWord8 x -> GV.Number $ realToFrac x
  VPWord16 x -> GV.Number $ realToFrac x
  VPWord32 x -> GV.Number $ realToFrac x
  VPWord64 x -> GV.Number $ realToFrac x
  VPBool b   -> GV.Boolean b
  VPFloat f -> GV.Number $ realToFrac f
  VPDouble d -> GV.Number d
  VPScientific s -> GV.Number $ Scientific.toRealFloat s
  VPChar d -> GV.Str $ toText [d]
  VPString s -> GV.Str $ toText s
  VPText t -> GV.Str t
  VPOH oh -> GV.Number $ realToFrac $ Heidi.onehotIx oh
  VPUnit -> GV.Str "()" -- ICK


rowToVLData :: Heidi.Row [Heidi.TC] Heidi.VP -> GV.Data
rowToVLData = undefined
