module Tag
  ( Tag(..)
  , getTag
  , getTags
  , tagsToMap
  ) where

import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.Map.Strict          as M
import           Data.Word
import           Lib                      (get32BitInteger,
                                           get32BitSynchsafeInteger, slice)
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
    content = slice 10 (size + 10) charArr

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

insertTag :: Tag -> M.Map String Tag -> M.Map String Tag
insertTag tag = M.insert (name tag) tag

deleteTag :: String -> M.Map String Tag -> M.Map String Tag
deleteTag = M.delete
  