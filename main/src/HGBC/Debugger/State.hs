{-# LANGUAGE TupleSections #-}
module HGBC.Debugger.State
  ( DebugState(..)
  , saveLabels
  , restoreLabels
  , saveBreakpoints
  , restoreBreakpoints
  )
where

import           Control.Exception              ( bracket )
import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           HGBC.Errors
import           Machine.GBC.Disassembler
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Read
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashTable.IO             as H
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

data DebugState = DebugState {
    disassemblyRef   :: IORef Disassembly
  , breakpoints      :: H.BasicHashTable LongAddress Bool
  , labelsRef        :: IORef (HM.HashMap LongAddress (T.Text, Bool))
  , romFileName      :: FilePath
  , bootDebuggerPath :: Maybe FilePath
  , romDebuggerPath  :: FilePath
}

labelsFileName :: FilePath
labelsFileName = "labels"

breakpointsFileName :: FilePath
breakpointsFileName = "breakpoints"

saveLabels :: DebugState -> IO ()
saveLabels debugState = do
  labels <- readIORef (labelsRef debugState)
  saveWithFilter (romDebuggerPath debugState) labels (\(LongAddress bank _) -> bank /= 0xFFFF)
  case bootDebuggerPath debugState of
    Nothing   -> pure ()
    Just path -> saveWithFilter path labels (\(LongAddress bank _) -> bank == 0xFFFF)
 where
  saveWithFilter labelsPath labels addressFilter = do
    createDirectoryIfMissing True labelsPath
    withTempFile labelsPath (labelsFileName <> ".tmp") $ \(file, handle) -> do
      for_ (filter (addressFilter . fst) (HM.toList labels))
        $ \(LongAddress bank offset, (text, isEditable)) -> when isEditable $ do
            hPutStr handle (show bank <> " " <> show offset <> " ")
            T.hPutStrLn handle text
      hClose handle
      renamePath file (labelsPath </> labelsFileName)

restoreLabels :: DebugState -> IO FileParseErrors
restoreLabels debugState = do
  errors0 <- readLabelsFile (romDebuggerPath debugState </> "labels")
  (errors0 <>) <$> case bootDebuggerPath debugState of
    Nothing   -> pure []
    Just path -> readLabelsFile (path </> "labels")
 where
  readLabelsFile path = do
    exists <- doesFileExist path
    if not exists
      then pure []
      else withFile path ReadMode $ \handle -> do
        contents <- hGetContents handle
        case parseLines contents of
          Left  errors -> pure [(path, errors)]
          Right labels -> [] <$ modifyIORef' (labelsRef debugState) (HM.fromList labels `HM.union`)
  parseLines contents =
    case partitionEithers $ writeError . second parseLine <$> [1 ..] `zip` lines contents of
      ([]    , labels) -> Right labels
      (errors, _     ) -> Left errors
  parseLine line = case words line of
    [bankRaw, offsetRaw, label] ->
      (LongAddress <$> readMaybe bankRaw <*> readMaybe offsetRaw) <&> (, (T.pack label, True))
    _ -> Nothing
  writeError (i, Nothing) = Left ("error on line " <> show (i :: Int))
  writeError (_, Just a ) = Right a

saveBreakpoints :: DebugState -> IO ()
saveBreakpoints debugState = do
  bps <- H.toList (breakpoints debugState)
  let path = romDebuggerPath debugState
  createDirectoryIfMissing True path
  withTempFile path (breakpointsFileName <> ".tmp") $ \(file, handle) -> do
    for_ bps $ \(LongAddress bank offset, isEnabled) ->
      hPutStrLn handle (show bank <> " " <> show offset <> " " <> show isEnabled)
    hClose handle
    renamePath file (path </> breakpointsFileName)

restoreBreakpoints :: DebugState -> IO FileParseErrors
restoreBreakpoints debugState = do
  let path = romDebuggerPath debugState </> breakpointsFileName
  exists <- doesFileExist path
  if not exists
    then pure []
    else withFile path ReadMode $ \handle -> do
      contents <- hGetContents handle
      case parseLines contents of
        Left  errors -> pure [(path, errors)]
        Right bps    -> [] <$ for_ bps (uncurry (H.insert (breakpoints debugState)))

 where
  parseLines contents =
    case partitionEithers $ writeError . second parseLine <$> [1 ..] `zip` lines contents of
      ([]    , labels) -> Right labels
      (errors, _     ) -> Left errors
  parseLine line = case words line of
    [bankRaw, offsetRaw, isEnabled] ->
      (,) <$> (LongAddress <$> readMaybe bankRaw <*> readMaybe offsetRaw) <*> readMaybe isEnabled
    _ -> Nothing
  writeError (i, Nothing) = Left ("error on line " <> show (i :: Int))
  writeError (_, Just a ) = Right a

withTempFile :: FilePath -> String -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile path template = bracket (openTempFile path template) cleanup
 where
  cleanup (file, handle) = do
    hClose handle
    exists <- doesFileExist file
    when exists $ removeFile file
