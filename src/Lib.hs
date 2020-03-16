module Lib
  ( get32BitSynchsafeInteger
  , slice
  , get32BitInteger
  , zeroFill8
  , zeroFill32
  , universalDecode
  ) where

import           Data.Char
import           Data.List
import qualified Data.Map.Strict          as M
import           Numeric                  (readInt, showHex, showIntAtBase)
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (c2w, w2c)
import           Data.Text                (Text)
import           Data.Word
import           Numbers
import           System.IO
import Data.Text.Encoding

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

zeroFill8 :: String -> String
zeroFill8 snum = replicate (8 - length snum) '0' ++ snum

zeroFill32 :: String -> String
zeroFill32 snum = replicate (32 - length snum) '0' ++ snum

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs

get32BitSynchsafeInteger :: (Integral a) => [a] -> Int
get32BitSynchsafeInteger charArr
  | all (== 0) charArr = 0
  | otherwise =
    read $ convertFromTo 2 10 $ concatMap (safeTail . zeroFill8 . convertFromTo 10 2 . show . toInteger) charArr

get32BitInteger :: (Integral a) => [a] -> Int
get32BitInteger charArr = read $ convertFromTo 2 10 $ concatMap (convertFromTo 10 2 . show . toInteger) charArr


universalDecode :: B.ByteString -> Text
universalDecode bytestring
  | B.pack [1, 255, 254] `B.isInfixOf` bytestring = decodeUtf16LE $ B.drop 3 bytestring
  | B.pack [2] `B.isInfixOf` bytestring = decodeUtf16BE $ B.drop 0 bytestring
  | B.pack [4] `B.isInfixOf` bytestring = decodeUtf8 $ B.drop 0 bytestring
  | otherwise = decodeLatin1 $ B.drop 0 bytestring