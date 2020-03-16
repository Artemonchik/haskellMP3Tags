module Tag
  ( Tag(..)
  , getTag
  , getTags
  , tagsToMap
  , convertTagToWord8Arr
  , convertTagsToWord8Arr
  ,insertTagToMap
  , createTag
  ) where

import qualified Data.ByteString          as B
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import           Data.Text.Encoding
import           Data.Word
import           Lib
import           Numbers
import           System.IO

data Tag =
  Tag
    { name    :: String
    , size    :: Int
    , flags   :: (Word8, Word8)
    , content :: [Word8]
    }
  deriving (Show)

clearAllTags :: Handle -> Integer
clearAllTags handle = 5

getTag :: [Word8] -> Either String Tag
getTag charArr
  | length charArr < 10 = Left "Frame is too short. Can't read the header"
  | size > length charArr - 10 =
    Left $
    "Frame requires more data than given: " ++
    "given " ++ show (length charArr) ++ ", but frame require: " ++ show (size + 10)
  | any (\x -> (x < 'A' || x > 'Z') && (x < '0' || x > '9')) name = Left "Incorrect name format (A-Z, 0-9)"
  | length charArr >= 10 = Right $ Tag name size flags content
  where
    name = map w2c (take 4 charArr)
    size = get32BitInteger (slice 4 7 charArr)
    flags = (charArr !! 8, charArr !! 9)
    content = slice 10 (size + 9) charArr

eitherSize :: Either String Tag -> Int
eitherSize (Left _)                 = (maxBound :: Int) - (500 :: Int)
eitherSize (Right (Tag _ size _ _)) = size

getTags :: [Word8] -> [Either String Tag]
getTags [] = []
getTags arrChar
  | head arrChar == 0 = []
  | otherwise = tag : getTags (drop (eitherSize tag + 10) arrChar)
  where
    tag = getTag arrChar

tagsToMap :: [Tag] -> M.Map String Tag
tagsToMap tags = M.fromList $ zip (map name tags) tags

createTag :: String -> (Word8, Word8) -> [Word8] -> Tag
createTag name flags content = Tag name (length content) flags content

insertTagToMap :: Tag -> M.Map String Tag -> M.Map String Tag
insertTagToMap tag = M.insert (name tag) tag

deleteTagFromMap :: String -> M.Map String Tag -> M.Map String Tag
deleteTagFromMap = M.delete

convertTagToWord8Arr :: Tag -> [Word8]
convertTagToWord8Arr (Tag name size flags content) = w8name ++ w8size ++ w8flags ++ content
  where
    w8name = map c2w name
    w8size =
      map
        (read . zeroFill8 . convertFromTo 2 10)
        [slice 0 7 ssize, slice 8 15 ssize, slice 16 23 ssize, slice 24 31 ssize] :: [Word8]
    w8flags = [fst flags, snd flags]
    ssize = zeroFill32 $ convertFromTo 10 2 (show size)

convertTagsToWord8Arr :: [Tag] -> [Word8]
convertTagsToWord8Arr = concatMap convertTagToWord8Arr
