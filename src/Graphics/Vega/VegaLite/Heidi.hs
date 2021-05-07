{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Vega.VegaLite.Heidi where

import qualified Data.Foldable as Foldable
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Graphics.Vega.VegaLite        as GV
import qualified Heidi
import Heidi (VP(..))


-- This should be in Heidi ??
type FRow k v r = Heidi.Row k (v -> r)

intersectionWithA:: (Applicative t, Heidi.TrieKey k) => (a -> b -> t r) -> Heidi.Row k a -> Heidi.Row k b -> t (Heidi.Row k r)
intersectionWithA f ra = Heidi.traverseWithKey (const id) . Heidi.intersectionWith f ra

applyFRow :: Heidi.TrieKey k => FRow k v r -> Heidi.Row k v -> Heidi.Row k r
applyFRow = Heidi.intersectionWith ($)

applyFRowA :: (Heidi.TrieKey k, Applicative t) => FRow k v (t r) -> Heidi.Row k v -> t (Heidi.Row k r)
applyFRowA = intersectionWithA ($)
-- end Heidi bits

vpTodvNumber :: VP -> Either Text GV.DataValue
vpTodvNumber = \case
  VPInt x -> Right $ GV.Number $ realToFrac x
  VPInt8 x -> Right $ GV.Number $ realToFrac x
  VPInt16 x -> Right $ GV.Number $ realToFrac x
  VPInt32 x -> Right $ GV.Number $ realToFrac x
  VPInt64 x -> Right $ GV.Number $ realToFrac x
  VPWord x -> Right $ GV.Number $ realToFrac x
  VPWord8 x -> Right $ GV.Number $ realToFrac x
  VPWord16 x -> Right $ GV.Number $ realToFrac x
  VPWord32 x -> Right $ GV.Number $ realToFrac x
  VPWord64 x -> Right $ GV.Number $ realToFrac x
  VPFloat f -> Right $ GV.Number $ realToFrac f
  VPDouble d -> Right $ GV.Number d
  VPScientific s -> Right $ GV.Number $ Scientific.toRealFloat s
  _ -> Left "Not a Numeric type in vpTodvNumber"

vpTodvBool :: VP -> Either Text GV.DataValue
vpTodvBool = \case
  VPBool b -> Right $ GV.Boolean b
  _ -> Left "Not a Boolean in vpTodvBool"

vpTodvStr :: VP -> Either Text GV.DataValue
vpTodvStr = \case
  VPChar d -> Right $ GV.Str $ toText [d]
  VPString s -> Right $ GV.Str $ toText s
  VPText t -> Right $ GV.Str t
  VPUnit -> Right $ GV.Str "()" -- ICK
  _ -> Left "Not a Text/String type in vpTodvStr"

-- this will require a type-application to specify @a@
-- should we also require @Bounded@ and check that we are in bounds?
vpTodvEnum :: forall a.(Enum a, Show a) => VP -> Either Text GV.DataValue
vpTodvEnum = \case
  VPOH oh -> Right $ GV.Str $ show $ toEnum @a $ Heidi.onehotIx oh
  _ -> Left "Not an enum (OneHot) type in vpTodvEnum"

-- hvega specific
tcToColName :: [Heidi.TC] -> Text
tcToColName tcs = T.intercalate "_" $ fmap (toText . Heidi.tcTyN) $ reverse tcs

hvegaF :: [Text] -> (VP -> Either Text GV.DataValue) -> ([Heidi.TC], VP -> Either Text GV.DataValue)
hvegaF colName toDV = (fmap (Heidi.mkTyN . toString) colName, toDV)

asNumber' :: [Text] -> ([Heidi.TC], VP -> Either Text GV.DataValue)
asNumber' ns = hvegaF ns vpTodvNumber

asNumber :: Text -> ([Heidi.TC], VP -> Either Text GV.DataValue)
asNumber n = asNumber' [n]

asStr' :: [Text] -> ([Heidi.TC], VP -> Either Text GV.DataValue)
asStr' ns = hvegaF ns vpTodvStr

asStr :: Text -> ([Heidi.TC], VP -> Either Text GV.DataValue)
asStr n = asStr' [n]

asBool' :: [Text] -> ([Heidi.TC], VP -> Either Text GV.DataValue)
asBool' ns = hvegaF ns vpTodvBool

asBool :: Text -> ([Heidi.TC], VP -> Either Text GV.DataValue)
asBool n = asBool' [n]

-- this will require a type-application to specify @a@
asEnum' :: forall a. (Enum a, Show a) => [Text] -> ([Heidi.TC], VP -> Either Text GV.DataValue)
asEnum' ns = hvegaF ns (vpTodvEnum @a)

-- this will require a type-application to specify @a@
asEnum :: forall a. (Enum a, Show a) => Text -> ([Heidi.TC], VP -> Either Text GV.DataValue)
asEnum n = asEnum' @a [n]

toVLDataRow :: Foldable f
            =>  f ([Heidi.TC], VP -> Either Text GV.DataValue)
            -> Heidi.Row [Heidi.TC] Heidi.VP
            -> Either Text [GV.DataRow]
toVLDataRow colFuncs row = do
  let frow = Heidi.rowFromList $ Foldable.toList colFuncs
  dvRow <- applyFRowA frow row
  return $ GV.dataRow (fmap (first tcToColName) $ Heidi.toList dvRow) []


rowsToVLData :: (Foldable f, Traversable g)
            => [(T.Text, GV.DataType)]
            -> f ([Heidi.TC], VP -> Either Text GV.DataValue)
            -> g (Heidi.Row [Heidi.TC] Heidi.VP)
            -> Either Text GV.Data
rowsToVLData parseList colFuncs rows =
  fmap (GV.dataFromRows [GV.Parse $ parseList] . concat . Foldable.toList) $ traverse (toVLDataRow colFuncs) rows


---
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
