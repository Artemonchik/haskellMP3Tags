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
import TagConvert
type State = IORef ([Text], Maybe Text, Ref Window, Ref TextBuffer, Ref TextBuffer)


chooseFileButtonCallback :: State -> M.Map Text (Ref TextBuffer) -> Text ->Ref Button -> IO()
chooseFileButtonCallback state' buffers name _ -- callback for button with file name
 = do
  (filenames, selectedFile, window, folderBuff, currTrackBuffer) <- readIORef state'
  setText currTrackBuffer name
  folderPath <- unpack <$> getText folderBuff
  fileName <- unpack <$> getText currTrackBuffer
  let fullPath = joinPath [folderPath, fileName]

  handle <- openFile (fullPath) ReadMode
  header <- B.hGet handle 10
  let tagLen = get32BitSynchsafeInteger $ slice 6 10 $ B.unpack header
  binBody <- B.hGet handle tagLen

  let fieldNames =  M.elems fieldsToTagNames
  let body = B.unpack binBody
  let tagNameValuesMap = M.map (universalDecode . B.pack . content)  $ M.filterWithKey  (\k v -> (k `elem` fieldNames)) $ tagsToMap $ rights $ getTags body
--  fout <- openFile "out.txt" WriteMode
--  hPrint fout tagNameValuesMap
  let buffersWithID3KeysList = map (\(key, buffer) -> (pack $ fromJust (M.lookup (unpack key) fieldsToTagNames), buffer))$ M.toList buffers
--  hPrint fout buffersWithID3KeysList
  let valueBufferList = [(buffer, value) | (key, buffer) <- buffersWithID3KeysList, (tagKey, value) <- (M.toList tagNameValuesMap), (pack tagKey) == key]
  _ <- mapM (\(buff, value) -> setText buff value ) valueBufferList
--  hClose fout
  hClose handle
  writeIORef state' (filenames, Just name, window, folderBuff, currTrackBuffer)

createFileNameButton :: State -> M.Map Text (Ref TextBuffer) -> (Text, Int) -> IO (Ref Button) -- Returns new button with file name
createFileNameButton state' buffers (name, index) = do
  (filenames, selectedFile, window, buff, currTrackBuffer) <- readIORef state'
  button <- buttonNew (toRectangle (35, 35 * index, 25, 25)) (Just name)
  setCallback button $ chooseFileButtonCallback state' buffers name
  return button

showListOfSongs :: State -> M.Map Text (Ref TextBuffer) -> IO ()
showListOfSongs state' buffers = do
  (filenames, selectedFile, window, buff, currTrackBuffer) <- readIORef state'
  begin window
  scrolled <- scrolledNew (toRectangle (30,100,240,485)) Nothing
  begin scrolled
  pack <- packNew (toRectangle (30, 100, 240, 485)) (Just "Choosen files")
  setBox pack DownFrame
  begin pack
  buttons <- mapM (createFileNameButton state' buffers) (zip filenames [1..])
  
  end pack
  end scrolled
  end window
  redraw window

getMP3NameListToState :: State -> M.Map Text (Ref TextBuffer) -> Ref Button -> IO ()
getMP3NameListToState state' buffers button = do
  (filenames, selectedFile, window, buffWithPath, currTrackBuffer) <- readIORef state'
  text <- getText buffWithPath
  list <- listDirectory (unpack text)
  writeIORef state' (filter (isSuffixOf (pack ".mp3")) (fmap pack list), selectedFile, window, buffWithPath, currTrackBuffer)
  showListOfSongs state' buffers
  return ()

createTagField :: State -> (Text, Int, Ref TextBuffer) -> IO (Ref TextEditor)
createTagField state' (name, index, buffer) = do
 tagFieldEditor <-
     textEditorNew
       (Rectangle (Position (X 300) (Y (90 + 70 * index))) (Size (Width 300) (Height 30)))
       (Just name)
 setBuffer tagFieldEditor (Just buffer)
 return tagFieldEditor
--   list <- listDi
main = do
  window' <- windowNew (Size (Width 700) (Height 500)) Nothing (Just "MP3 Tags")
  begin window'

  folderBuff <- textBufferNew Nothing Nothing
  currentTrackBuff <- textBufferNew Nothing Nothing

  state' <- newIORef ([] :: [Text], Nothing :: Maybe Text, window', folderBuff, currentTrackBuff)
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

  let fieldNames = fmap pack $ M.keys fieldsToTagNames
  _ <- textEditorNew (toRectangle (300, 100, 0, 0)) (Nothing)
  tagValueBuffers <- mapM (\_ -> textBufferNew Nothing Nothing) fieldNames
  textField <- mapM (createTagField state') (zip3 fieldNames [1..] tagValueBuffers)
  

  end fieldsPack
  end scrolled

  mp3FilesButton <- buttonNew (Rectangle (Position (X 530) (Y 60)) (Size (Width 100) (Height 30))) (Just (pack "Open folder"))
  setCallback mp3FilesButton (getMP3NameListToState state' (M.fromList $ zip fieldNames tagValueBuffers))

  end window'
  showWidget window'
  _ <- FL.run
  return ()
