module Fields
  ( fieldsToTagNames
  , tagNamesToFields
  ) where

import qualified Data.Map.Strict as M

fieldsToTagNames :: M.Map String String
fieldsToTagNames =
  M.fromList
    [ ("Album", "TALB")
    , ("Author", "TPE1")
    , ("Date", "TDAT")
    , ("Text", "TEXT")
    , ("year", "TYER")
    , ("Original name", "TIT2")
    ]

--    , ("Comment", "COMM")
tagNamesToFields :: M.Map String String
tagNamesToFields = M.fromList [(val, key) | (key, val) <- M.toList fieldsToTagNames]
