{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module HGBC.Debugger.State
  ( DebugState(..)
  , init
  , saveLabels
  , restoreLabels
  , saveBreakpoints
  , restoreBreakpoints
  )
where

import           Control.Exception       hiding ( handle )
import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           HGBC.Errors
import           Machine.GBC.Disassembler
import           Prelude                 hiding ( init )
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Read
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashTable.IO             as H
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as LB
import           Data.Time.Clock
import qualified HGBC.Config.Paths             as Path
import qualified HGBC.Debugger.SymFile         as Sym

data DebugState = DebugState {
    disassemblyRef   :: IORef Disassembly
  , breakpoints      :: H.BasicHashTable LongAddress Bool
  , labelsRef        :: IORef (HM.HashMap LongAddress (T.Text, Bool))
  , romFileName      :: FilePath
  , romFilePath      :: FilePath
  , bootDebuggerPath :: Maybe FilePath
  , romDebuggerPath  :: FilePath
}

-- | Initialize the debugger state.
init :: FilePath -> Maybe FilePath -> IO DebugState
init rom bootROM = do
  let romFileName = takeBaseName rom
  let romFilePath = takeDirectory rom
  disassemblyRef   <- newIORef mempty
  breakpoints      <- H.new
  labelsRef        <- newIORef (HM.fromList initialLabels)
  bootDebuggerPath <- traverse Path.debugState bootROM
  romDebuggerPath  <- Path.debugState romFileName
  pure DebugState { .. }

labelsFileName :: FilePath
labelsFileName = "labels"

breakpointsFileName :: FilePath
breakpointsFileName = "breakpoints"

-- | Persist the state of the debugger labels.
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

      LB.hPutStr handle
        . Sym.generate
        . fmap (second fst)
        . filter (snd . snd)
        . filter (addressFilter . fst)
        $ HM.toList labels

      hClose handle
      renamePath file (labelsPath </> labelsFileName)

-- | Restore the persisted debugger labels.
restoreLabels :: DebugState -> IO [FileParseErrors]
restoreLabels debugState = do
  errors0 <- readLabelsFile (romDebuggerPath debugState </> "labels")
  (errors0 <>) <$> case bootDebuggerPath debugState of
    Nothing   -> pure []
    Just path -> readLabelsFile (path </> "labels")
 where
  readLabelsFile path = do
    let symPath = romFilePath debugState </> romFileName debugState -<.> "sym"
    labelsExists <- doesFileExist path
    symExists    <- doesFileExist symPath
    labelsTime   <- if labelsExists then Just <$> getModificationTime path else pure Nothing
    symTime      <- if symExists then Just <$> getModificationTime symPath else pure Nothing
    let symNewer = maybe False (> 0) (diffUTCTime <$> symTime <*> labelsTime)

    if symNewer || (not labelsExists && symExists)
      then copyDefaultSymFile path symPath
      else
        fmap (either (\e -> [(path, [displayIOException e])]) id)
        . try
        $ withFile path ReadMode
        $ \handle -> do
            (errors, labels) <- partitionEithers . Sym.parse <$> LB.hGetContents handle
            modifyIORef' (labelsRef debugState)
                         (HM.fromList (second (, True) <$> labels) `HM.union`)
            pure ([ (path, errors) | not (null errors) ])
  copyDefaultSymFile path symPath = do
    r <- try $ do
      createDirectoryIfMissing True (takeDirectory path)
      copyFile symPath path
    case r of
      Right ()  -> readLabelsFile path
      Left  err -> pure [(symPath, [displayIOException err])]

displayIOException :: IOException -> String
displayIOException = displayException

-- | Persist the breakpoints state.
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

-- | Restore the persisted breakpoints.
restoreBreakpoints :: DebugState -> IO [FileParseErrors]
restoreBreakpoints debugState = do
  let path = romDebuggerPath debugState </> breakpointsFileName
  exists <- doesFileExist path
  if not exists
    then pure []
    else
      fmap (either (\e -> [(path, [displayIOException e])]) id)
      . try
      $ withFile path ReadMode
      $ \handle -> do
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
