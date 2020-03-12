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
  hPrint stdout $ tagsToMap $ rights $ getTags body
  return ()

--{-# LANGUAGE OverloadedStrings #-}
--module Main where
--import qualified Graphics.UI.FLTK.LowLevel.FL as FL
--import Graphics.UI.FLTK.LowLevel.Fl_Types
--import Graphics.UI.FLTK.LowLevel.FLTKHS
--
--buttonCb :: Ref Button -> IO ()
--buttonCb b' = do
--  l' <- getLabel b'
--  if (l' == "Hello world")
--    then setLabel b' "Goodbye world"
--    else setLabel b' "Hello world"
--
--ui :: IO ()
--ui = do
-- window <- windowNew
--           (Size (Width 115) (Height 100))
--           Nothing
--           Nothing
-- begin window
-- b' <- buttonNew
--        (Rectangle (Position (X 10) (Y 30)) (Size (Width 95) (Height 30)))
--        (Just "Hello world")
-- setLabelsize b' (FontSize 10)
-- setCallback b' buttonCb
-- end window
-- showWidget window
--
--main :: IO ()
--main = ui >> FL.run >> FL.flush
--
--replMain :: IO ()
--replMain = ui >> FL.replRun
