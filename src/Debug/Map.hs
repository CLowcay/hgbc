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
import           Data.Foldable
import           Data.List
import           Data.Word
import           GBC.Registers
import           System.Directory
import           System.FilePath
import           System.IO
import qualified Data.HashMap.Strict           as HM

-- | Add a symbol to the code map.
addToMap :: (String, Word16) -> SymbolTable -> SymbolTable
addToMap (label, value) (SymbolTable to from) =
  SymbolTable (HM.insert value label to) (HM.insert label value from)

-- | Remove a symbol from the code map.
removeFromMap :: String -> SymbolTable -> SymbolTable
removeFromMap label (SymbolTable to from) = case HM.lookup label from of
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
  for_ (listSymbols codeMap) $ \(label, value) -> hPutStrLn hFile $ label ++ "," ++ show value

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
    [ (P1   , "P1")
    , (SB   , "SB")
    , (SC   , "SC")
    , (DIV  , "DIV")
    , (TIMA , "TIMA")
    , (TMA  , "TMA")
    , (TAC  , "TAC")
    , (IF   , "IF")
    , (NR10 , "NR10")
    , (NR11 , "NR11")
    , (NR12 , "NR12")
    , (NR13 , "NR13")
    , (NR14 , "NR14")
    , (NR21 , "NR21")
    , (NR22 , "NR22")
    , (NR23 , "NR23")
    , (NR24 , "NR24")
    , (NR30 , "NR30")
    , (NR31 , "NR31")
    , (NR32 , "NR32")
    , (NR33 , "NR33")
    , (NR34 , "NR34")
    , (NR41 , "NR41")
    , (NR42 , "NR42")
    , (NR43 , "NR43")
    , (NR44 , "NR44")
    , (NR50 , "NR50")
    , (NR51 , "NR51")
    , (NR52 , "NR52")
    , (LCDC , "LCDC")
    , (STAT , "STAT")
    , (SCY  , "SCY")
    , (SCX  , "SCX")
    , (LY   , "LY")
    , (LYC  , "LYC")
    , (DMA  , "DMA")
    , (BGP  , "BGP")
    , (OBP0 , "OBP0")
    , (OBP1 , "OBP1")
    , (WY   , "WY")
    , (WX   , "WX")
    , (KEY1 , "KEY1")
    , (VBK  , "VBK")
    , (HDMA1, "HDMA1")
    , (HDMA2, "HDMA2")
    , (HDMA3, "HDMA3")
    , (HDMA4, "HDMA4")
    , (HDMA5, "HDMA5")
    , (RP   , "RP")
    , (BCPS , "BCPS")
    , (BCPD , "BCPD")
    , (OCPS , "OCPS")
    , (OCPD , "OCPD")
    , (SVBK , "SVBK")
    , (IE   , "IE")
    ]




