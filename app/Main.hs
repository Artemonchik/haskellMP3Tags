{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString                           as B
import           Data.ByteString.Internal                  (c2w, w2c)
import           Data.Either
import           Data.IORef
import           Data.Text                                 hiding (filter, map,
                                                            zip)
import Data.Maybe
import           Foreign.C.Types
import qualified Graphics.UI.FLTK.LowLevel.FL              as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import           Graphics.UI.FLTK.LowLevel.Fl_Types
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Lib
import           Numbers
import           Numeric                                   (showHex)
import           System.Directory
import           System.IO
import           Tag                                       (getTags, tagsToMap, content)
import Fields
import qualified Data.Map.Strict as M
import System.FilePath.Posix
import Data.Text.Encoding
import Components

type State = IORef ([Text], Ref Window, Ref TextBuffer, Ref TextBuffer, [Ref TextEditor])

main = do
  window' <- windowNew (Size (Width 700) (Height 500)) Nothing (Just "MP3 Tags")
  begin window'

  folderBuff <- textBufferNew Nothing Nothing
  currentTrackBuff <- textBufferNew Nothing Nothing

  state' <- newIORef ([] :: [Text], window', folderBuff, currentTrackBuff, [] :: [Ref TextEditor])

  chooseFolderField <-
    textEditorNew
      (Rectangle (Position (X 30) (Y 30)) (Size (Width 600) (Height 30)))
      (Just (pack "DO Specify the path to the folder"))
  setBuffer chooseFolderField (Just folderBuff)

  currentTrackField <- textDisplayNew (Rectangle (Position (X 300) (Y 440)) (Size (Width 340) (Height 50))) (Just (pack "Current Track"))
  setBuffer currentTrackField (Just currentTrackBuff)


  scrolled <- scrolledNew (toRectangle (300,110,370,285)) Nothing
  begin scrolled
  fieldsPack <- packNew (toRectangle (300, 110, 340, 985)) (Just "Tags")
  setBox fieldsPack DownFrame
  setSpacing fieldsPack 30
  begin fieldsPack

  let fieldNames = pack <$> M.keys fieldNameToFrameName
  _ <- textEditorNew (toRectangle (300, 100, 0, 0)) Nothing
  frameEditorFieldBuffers <- mapM (\_ -> textBufferNew Nothing Nothing) fieldNames
  frameEditorFields <- mapM createTagFrameEditField (zip3 fieldNames [1..] frameEditorFieldBuffers)
  (filenames, window, buffWithPath, currTrackBuffer, _) <- readIORef state'
  writeIORef state' (filenames, window, buffWithPath, currTrackBuffer, frameEditorFields)
  end fieldsPack
  end scrolled

  mp3FilesButton <- buttonNew (Rectangle (Position (X 530) (Y 60)) (Size (Width 100) (Height 30))) (Just (pack "Open folder"))
  setCallback mp3FilesButton (writeMP3NameListToStateAndCreateButtonsAndAddToWindow state')
  
  saveTagsButton <- buttonNew (Rectangle (Position (X 30) (Y 440)) (Size (Width 200) (Height 50))) (Just (pack "Save tags"))
  setCallback saveTagsButton (saveTagsToFile state')
  end window'
  showWidget window'
  _ <- FL.run
  return ()
