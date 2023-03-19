{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Vega.VegaLite.JSON
  (
    rowsToJSON
  , rowsToJSONM
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import qualified Control.Foldl as FL
import qualified Graphics.Vega.VegaLite        as GV



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



formatProperty :: GV.Format -> [AT.Pair]
formatProperty (GV.JSON js) =
  let ps = [("type", "json")]
           <> if T.null (T.strip js) then [] else [("property", js)]
  in map (second A.toJSON) ps
