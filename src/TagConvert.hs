module TagConvert
  ( convertTagToWord8Arr
  , convertTagsToWord8Arr
  , universalDecode
  ) where

import qualified Data.ByteString          as B
import           Data.ByteString.Internal (c2w, w2c)
import           Data.Text                (Text)
import           Data.Word
import           Lib
import           Numbers
import           System.IO
import           Tag                      hiding (content, flags, name, size)
import Data.Text.Encoding

universalDecode :: B.ByteString -> Text
universalDecode bytestring
  | B.pack [1, 255, 254] `B.isInfixOf` bytestring = decodeUtf16LE $ B.drop 3 bytestring
  | B.pack [2] `B.isInfixOf` bytestring = decodeUtf16BE $ B.drop 0 bytestring
  | B.pack [4] `B.isInfixOf` bytestring = decodeUtf8 $ B.drop 0 bytestring
  | otherwise = decodeLatin1 $ B.drop 0 bytestring

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
