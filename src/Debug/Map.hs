module Debug.Map
  ( addToMap
  , removeFromMap
  , listSymbols
  , mapFileName
  , saveMap
  , loadMap
  , initMap
  )
where

import           Common
import           Control.Monad
import           Data.List
import           Data.Word
import           System.FilePath
import           System.IO
import           System.Directory
import qualified Data.HashMap.Strict           as HM

-- | Add a symbol to the code map.
addToMap :: SymbolTable -> (String, Word16) -> SymbolTable
addToMap (SymbolTable to from) (label, value) =
  SymbolTable (HM.insert value label to) (HM.insert label value from)

-- | Remove a symbol from the code map.
removeFromMap :: SymbolTable -> String -> SymbolTable
removeFromMap (SymbolTable to from) label = case HM.lookup label from of
  Nothing    -> SymbolTable to (HM.delete label from)
  Just value -> SymbolTable (HM.delete value to) (HM.delete label from)

-- | List all the symbols in the code map.
listSymbols :: SymbolTable -> [(String, Word16)]
listSymbols (SymbolTable _ allSymbols) =
  let SymbolTable _ builtIn = defaultCodeMap
  in  sortOn snd . HM.toList $ allSymbols `HM.difference` builtIn

-- | Get the default name for the map file for a ROM file.
mapFileName :: FilePath -> FilePath
mapFileName romPath = romPath <.> ".map"

-- | Save the map file.
saveMap :: FilePath -> SymbolTable -> IO ()
saveMap filePath codeMap = withFile filePath WriteMode $ \hFile ->
  forM_ (listSymbols codeMap) $ \(label, value) -> hPutStrLn hFile $ label ++ "," ++ show value

-- | Load the map file.
loadMap :: FilePath -> IO SymbolTable
loadMap filePath = do
  let SymbolTable to from = defaultCodeMap
  symbols <- fmap (fmap (read . tail) . break (== ',')) . lines <$> readFile filePath
  pure $ SymbolTable (to `HM.union` HM.fromList (swap <$> symbols))
                     (from `HM.union` HM.fromList symbols)
  where swap (a, b) = (b, a)

-- | Initialize the code map for a given ROM file name.
initMap :: FilePath -> IO SymbolTable
initMap romFile = do
  hasMapFile <- doesFileExist $ mapFileName romFile
  if hasMapFile then loadMap (mapFileName romFile) else pure defaultCodeMap

-- | The built-in code map.
defaultCodeMap :: SymbolTable
defaultCodeMap = SymbolTable (HM.fromList tableData) (HM.fromList $ swap <$> tableData)
 where
  swap (x, y) = (y, x)
  tableData =
    [ (0xFF00, "P1")
    , (0xFF01, "SB")
    , (0xFF01, "SB")
    , (0xFF02, "SC")
    , (0xFF04, "DIV")
    , (0xFF05, "TIMA")
    , (0xFF06, "TMA")
    , (0xFF07, "TAC")
    , (0xFF0F, "IF")
    , (0xFF10, "NR10")
    , (0xFF11, "NR11")
    , (0xFF12, "NR12")
    , (0xFF13, "NR13")
    , (0xFF14, "NR14")
    , (0xFF16, "NR21")
    , (0xFF17, "NR22")
    , (0xFF18, "NR23")
    , (0xFF19, "NR24")
    , (0xFF1A, "NR30")
    , (0xFF1B, "NR31")
    , (0xFF1C, "NR32")
    , (0xFF1D, "NR33")
    , (0xFF1E, "NR34")
    , (0xFF20, "NR41")
    , (0xFF21, "NR42")
    , (0xFF22, "NR43")
    , (0xFF23, "NR44")
    , (0xFF24, "NR50")
    , (0xFF25, "NR51")
    , (0xFF26, "NR52")
    , (0xFF40, "LCDC")
    , (0xFF41, "STAT")
    , (0xFF42, "SCY")
    , (0xFF43, "SCX")
    , (0xFF44, "LY")
    , (0xFF45, "LYC")
    , (0xFF46, "DMA")
    , (0xFF47, "BGP")
    , (0xFF48, "OBP0")
    , (0xFF49, "OBP1")
    , (0xFF4A, "WY")
    , (0xFF4B, "WX")
    , (0xFF4D, "KEY1")
    , (0xFF4F, "VBK")
    , (0xFF51, "HDMA1")
    , (0xFF52, "HDMA2")
    , (0xFF53, "HDMA3")
    , (0xFF54, "HDMA4")
    , (0xFF55, "HDMA5")
    , (0xFF56, "RP")
    , (0xFF68, "BCPS")
    , (0xFF69, "BCPD")
    , (0xFF6A, "OCPS")
    , (0xFF6B, "OCPD")
    , (0xFF70, "SVBK")
    , (0xFFFF, "IE")
    ]




