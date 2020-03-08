module Lib
  ( get32BitSynchsafeInteger
  , slice
  , get32BitInteger
  ) where

import qualified Data.ByteString as B
import           Data.Char
import           Data.List
import           Data.Word
import           Numbers
import           Numeric         (readInt, showHex, showIntAtBase)
import           System.IO
import qualified Data.Map as M

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

zeroFill :: String -> String
zeroFill snum = replicate (8 - length snum) '0' ++ snum

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs

get32BitSynchsafeInteger :: (Integral a) => [a] -> Int
get32BitSynchsafeInteger charArr =
  read $ convertFromTo 2 10 $ concatMap (safeTail . zeroFill . convertFromTo 10 2 . show . toInteger) charArr

get32BitInteger :: (Integral a) => [a] -> Int
get32BitInteger charArr = read $ convertFromTo 2 10 $ concatMap (convertFromTo 10 2 . show . toInteger) charArr