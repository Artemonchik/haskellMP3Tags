{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString          as B
import           Data.ByteString.Internal (c2w, w2c)
import           Data.Either
import           Lib
import           Numbers
import           Numeric                  (showHex)
import           System.IO
import           Tag                      (getTags, tagsToMap)

main :: IO ()
main = do
  handle <- openBinaryFile "C:/Users/Artem/HaskellProjects/haskellMP3Tags/src/music/LSP.mp3" ReadMode
  header <- B.hGet handle 10
  let tagLen = get32BitSynchsafeInteger $ slice 6 10 $ B.unpack header
  print tagLen
  binBody <- B.hGet handle tagLen
  let body = B.unpack binBody
  print body
  print $ tagsToMap $ rights $ getTags body
  return ()