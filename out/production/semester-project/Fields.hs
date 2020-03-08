module Fields where

import qualified Data.Map as M

frames :: M.Map String String
frames =
  M.fromList
    [ ("TALB", "Album")
    , ("TEXT", "Author")
    , ("TDAT", "Date")
    , ("TEXT", "Text")
    , ("TYER", "Year")
    , ("TIT2", "Original name")
    , ("COMM", "Comment")
    ]
