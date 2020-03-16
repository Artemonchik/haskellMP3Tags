{-# LANGUAGE OverloadedStrings #-}

module Components
  ( createTagFrameEditField
  , writeMP3NameListToStateAndCreateButtonsAndAddToWindow
  , saveTagsToFile
  ) where

import           Control.Monad
import qualified Data.ByteString                           as B
import           Data.ByteString.Internal                  (c2w, w2c)
import           Data.Either
import           Data.IORef
import qualified Data.Map.Strict                           as M
import           Data.Maybe
import           Data.Text                                 hiding (filter,
                                                            foldl, map, zip)
import           Data.Text.Encoding
import           Fields
import           Foreign.C.Types
import qualified Graphics.UI.FLTK.LowLevel.FL              as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import           Graphics.UI.FLTK.LowLevel.Fl_Types
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Lib
import           Numbers
import           Numeric                                   (showHex)
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           Tag

type State = IORef ([Text], Ref Window, Ref TextBuffer, Ref TextBuffer, [Ref TextEditor])

createTagFrameEditField :: (Text, Int, Ref TextBuffer) -> IO (Ref TextEditor)
createTagFrameEditField (name, index, buffer) = do
  tagFieldEditor <-
    textEditorNew (Rectangle (Position (X 300) (Y (90 + 70 * index))) (Size (Width 300) (Height 30))) (Just name)
  setBuffer tagFieldEditor (Just buffer)
  return tagFieldEditor

chooseFileButtonCallback :: State -> Text -> Ref Button -> IO ()
chooseFileButtonCallback state' name _ -- callback for button with file name
 = do
  (filenames, window, folderBuff, currTrackBuffer, frameEditorFields) <- readIORef state'
  setText currTrackBuffer name
  folderPath <- unpack <$> getText folderBuff
  fileName <- unpack <$> getText currTrackBuffer
  let fullPath = joinPath [folderPath, fileName]
  handle <- openFile fullPath ReadMode
  header <- B.hGet handle 10
  let tagLen = get32BitSynchsafeInteger $ slice 6 10 $ B.unpack header
  binBody <- B.hGet handle tagLen
  let fieldNames = M.elems fieldNameToFrameName
  let body = B.unpack binBody
  let tagNameValuesMap =
        M.map (universalDecode . B.pack . content) $
        M.filterWithKey (\k v -> k `elem` fieldNames) $ tagsToMap $ rights $ getTags body
  labels <- mapM getLabel frameEditorFields
  buffers <- map fromJust <$> mapM getBuffer frameEditorFields
  mapM_ (`setText` "") buffers
  let buffersWithID3KeysList =
        map (\(key, buffer) -> (pack $ fromJust (M.lookup (unpack key) fieldNameToFrameName), buffer)) $
        zip labels buffers
  let valueBufferList =
        [ (buffer, value)
        | (key, buffer) <- buffersWithID3KeysList
        , (tagKey, value) <- M.toList tagNameValuesMap
        , pack tagKey == key
        ]
  mapM_ (\(buff, value) -> setText buff value) valueBufferList
  hClose handle

--  hPrint fout buffersWithID3KeysList
--  writeIORef state' (filenames, window, folderBuff, currTrackBuffer, frameEditorFields)
createFileNameButton :: State -> (Text, Int) -> IO (Ref Button) -- Returns new button with file name
createFileNameButton state' (name, index) = do
  (filenames, window, buff, currTrackBuffer, frameEditorFields) <- readIORef state'
  button <- buttonNew (toRectangle (35, 35 * index, 25, 25)) (Just name)
  setCallback button $ chooseFileButtonCallback state' name
  return button

createButtonListOfSongs :: State -> IO [Ref Button]
createButtonListOfSongs state' = do
  (filenames, window, buff, currTrackBuffer, frameEditorFields) <- readIORef state'
  buttons <- mapM (createFileNameButton state') (zip filenames [1 ..])
  return buttons

writeMP3NameListToStateAndCreateButtonsAndAddToWindow :: State -> Ref Button -> IO ()
writeMP3NameListToStateAndCreateButtonsAndAddToWindow state' buffers = do
  (filenames, window, buffWithPath, currTrackBuffer, frameEditorFields) <- readIORef state'
  text <- getText buffWithPath
  list <- listDirectory (unpack text)
  writeIORef
    state'
    (filter (isSuffixOf (pack ".mp3")) (fmap pack list), window, buffWithPath, currTrackBuffer, frameEditorFields)
  begin window
  scrolled <- scrolledNew (toRectangle (30, 100, 240, 335)) Nothing
  begin scrolled
  pack <- packNew (toRectangle (30, 100, 240, 335)) (Just "Choosen files")
  setBox pack DownFrame
  begin pack
  buttons <- createButtonListOfSongs state'
  end pack
  end scrolled
  redraw window
  return ()

getTagsFromEditorFields :: [Ref TextEditor] -> IO [Tag]
getTagsFromEditorFields frameEditFields = do
  labels <- mapM getLabel frameEditFields
  maybeBuffers <- mapM getBuffer frameEditFields
  let buffers = fromJust <$> maybeBuffers
  values <- mapM getText buffers
  return $
    map (\(label, value) -> createTag (unpack label) (0, 0) (([1, 255, 254] ++) $ B.unpack $ encodeUtf16LE value)) $
    zip (map (\label -> pack $ fromJust $ M.lookup (unpack label) fieldNameToFrameName) labels) $ values

saveTagsToFile :: State -> Ref Button -> IO ()
saveTagsToFile state' _ = do
  (filenames, window, buffWithPath, currTrackBuffer, frameEditorFields) <- readIORef state'
  folderPath <- unpack <$> getText buffWithPath
  fileName <- unpack <$> getText currTrackBuffer
  let fullPath = joinPath [folderPath, fileName]
  handle <- openBinaryFile fullPath ReadMode
  header <- B.hGet handle 10
  let tagLen = get32BitSynchsafeInteger $ slice 6 10 $ B.unpack header
  binBody <- B.hGet handle tagLen
  hClose handle
  let body = B.unpack binBody
  let fileTagMap = tagsToMap $ rights $ getTags body
  fieldFrameTags <- getTagsFromEditorFields frameEditorFields
--  let fieldFrameTagsMap = tagsToMap $ fieldFrameTags
  let resultMapping = foldl (\mapping tag -> insertTagToMap tag mapping) fileTagMap fieldFrameTags
  fOut <- openFile "out.txt" WriteMode
  hPrint fOut resultMapping
  hClose fOut
  let word8result = convertTagsToWord8Arr $ M.elems resultMapping
  wHandle <- openFile fullPath ReadWriteMode
  hSeek wHandle AbsoluteSeek 10
  B.hPut wHandle $ B.pack word8result
  return ()