{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Vega.VegaLite.JSON
  (
    rowsToJSON
  , rowsToJSON'
  , rowsToJSONM
  , rowsToJSONM'
  , mergeJSONRows
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import qualified Control.Foldl as FL
import qualified Graphics.Vega.VegaLite        as GV
import qualified Data.Vector as V
import Data.Functor.Identity (Identity)


rowsToJSON' :: Foldable g => (row -> g [(GV.FieldName, GV.DataValue)]) -> [GV.Format] -> Maybe Text -> FL.Fold row A.Value
rowsToJSON' rowToGVFields gvFormat jsonKeyM = FL.premap rowToGVFields $ FL.Fold step begin end where
  jsonKey = fromMaybe "values" jsonKeyM
  step dataRowList gvFields' = foldl' (\rows gvFields -> GV.dataRow gvFields rows) dataRowList gvFields'
  begin = []
  end dataRowList = A.object
                    $ (AK.fromText jsonKey, A.toJSON dataRowList )
                    : if null gvFormat
                      then []
                      else [(AK.fromText "format", A.object (concatMap formatProperty gvFormat))]


rowsToJSON :: (row -> [(GV.FieldName, GV.DataValue)]) -> [GV.Format] -> Maybe Text -> FL.Fold row A.Value
rowsToJSON f = rowsToJSON' (Identity . f)

getRows :: Text -> A.Value -> Maybe (V.Vector A.Value)
getRows k = \case
  A.Object km -> case AKM.lookup (AK.fromText k) km of
    Just (A.Array a) -> pure a
    _ -> Nothing
  _ -> Nothing

getFmt :: A.Value -> Maybe A.Value
getFmt = \case
  A.Object km -> AKM.lookup (AK.fromText "format") km
  _ -> Nothing


mergeJSONRows :: Maybe Text -> A.Value -> A.Value -> Maybe A.Value
mergeJSONRows mKey aVal bVal = do
  let key = fromMaybe "values" mKey
  aVec <- getRows key aVal
  bVec <- getRows key bVal
  pure $ A.object $ [(AK.fromText key, (A.Array $ V.concat [aVec, bVec]))] <> maybe [] (pure . (AK.fromText "format",)) (getFmt aVal)

{-
rowsToJSON :: (row -> [(GV.FieldName, GV.DataValue)]) -> [GV.Format] -> Maybe Text -> FL.Fold row A.Value
rowsToJSON rowToGVFields gvFormat jsonKeyM = FL.premap rowToGVFields $ FL.Fold step begin end where
  jsonKey = fromMaybe "values" jsonKeyM
  step dataRowList gvFields = GV.dataRow gvFields dataRowList
  begin = []
  end dataRowList = A.object
                    $ (AK.fromText jsonKey, A.toJSON dataRowList )
                    : if null gvFormat
                      then []
                      else [(AK.fromText "format", A.object (concatMap formatProperty gvFormat))]
-}

rowsToJSONM' :: (Monad m, Foldable g) => (row -> m (g [(GV.FieldName, GV.DataValue)])) -> [GV.Format] -> Maybe Text -> FL.FoldM m row A.Value
rowsToJSONM' rowToGVFieldsM gvFormat jsonKeyM = FL.premapM rowToGVFieldsM $ FL.FoldM step begin end where
  jsonKey = fromMaybe "values" jsonKeyM
  step dataRowList gvFields' = pure $ foldl' (\rows gvFields -> GV.dataRow gvFields rows) dataRowList gvFields'
  begin = pure []
  end dataRowList = pure
                    $ A.object
                    $ (AK.fromText jsonKey, A.toJSON dataRowList )
                    : if null gvFormat
                      then []
                      else [(AK.fromText "format", A.object (concatMap formatProperty gvFormat))]


rowsToJSONM :: Monad m => (row -> m [(GV.FieldName, GV.DataValue)]) -> [GV.Format] -> Maybe Text -> FL.FoldM m row A.Value
rowsToJSONM f = rowsToJSONM' (fmap Identity . f)

{-
rowsToJSONM :: Monad m => (row -> m [(GV.FieldName, GV.DataValue)]) -> [GV.Format] -> Maybe Text -> FL.FoldM m row A.Value
rowsToJSONM rowToGVFieldsM gvFormat jsonKeyM = FL.premapM rowToGVFieldsM $ FL.FoldM step begin end where
  jsonKey = fromMaybe "values" jsonKeyM
  step dataRowList gvFields = pure $ GV.dataRow gvFields dataRowList
  begin = pure []
  end dataRowList = pure
                    $ A.object
                    $ (AK.fromText jsonKey, A.toJSON dataRowList )
                    : if null gvFormat
                      then []
                      else [(AK.fromText "format", A.object (concatMap formatProperty gvFormat))]
-}


formatProperty :: GV.Format -> [AT.Pair]
formatProperty (GV.JSON js) =
  let ps = [("type", "json")]
           <> if T.null (T.strip js) then [] else [("property", js)]
  in map (second A.toJSON) ps
