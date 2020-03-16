module Fields
  ( fieldNameToFrameName
  , frameNameToFieldName
  ) where

import qualified Data.Map.Strict as M

fieldNameToFrameName :: M.Map String String
fieldNameToFrameName =
  M.fromList
    [ ("Album", "TALB")
    , ("Artist", "TPE1")
    , ("Date", "TDAT")
    , ("Text", "TEXT")
    , ("year", "TYER")
    , ("Title", "TIT2")
    , ("Album Artist", "TPE2")
--    , ("Comment", "COMM")
    ]

--    , ("Comment", "COMM")
frameNameToFieldName :: M.Map String String
frameNameToFieldName = M.fromList [(val, key) | (key, val) <- M.toList fieldNameToFrameName]
