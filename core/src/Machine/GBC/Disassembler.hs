{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Machine.GBC.Disassembler
  ( LongAddress (..),
    Parameter (..),
    Instruction (..),
    Field (..),
    Editable,
    Labels,
    fieldAddress,
    fieldBytes,
    encodeAddress,
    Disassembly,
    DisassemblyState (..),
    lookupN,
    disassemblyRequired,
    disassembleROM,
    generateOutput,
    initialLabels,
    disassembleFrom,
    disassembleFromRoots,
    disassemble,
  )
where

import Control.Category hiding ((.))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bits
import qualified Data.ByteString.Short as SB
import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Hashable
import Data.Int
import qualified Data.IntMap.Lazy as IM
import Data.List (intersperse)
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Vector.Storable as VS
import Data.Word
import Machine.GBC.CPU.Decode
import Machine.GBC.CPU.ISA
import Machine.GBC.Disassembler.LabelGenerator
import qualified Machine.GBC.Memory as Memory
import Machine.GBC.Registers
import Machine.GBC.Util
import Prelude hiding (lookup)

data LongAddress
  = LongAddress !Word16 !Word16
  deriving (Eq, Ord, Show)

instance Hashable LongAddress where
  hashWithSalt salt (LongAddress bank offset) =
    hashWithSalt salt ((fromIntegral bank .<<. 16) .|. fromIntegral offset :: Int)

addOffset :: Integral a => LongAddress -> a -> LongAddress
addOffset (LongAddress bank offset0) offset = LongAddress bank (offset0 + fromIntegral offset)

type Overlap = Bool

data Field
  = Field LongAddress SB.ShortByteString Overlap Instruction
  deriving (Eq, Ord, Show)

data Instruction
  = Data
  | Instruction0 T.Text
  | Instruction1 T.Text Parameter
  | Instruction2 T.Text Parameter Parameter
  deriving (Eq, Ord, Show)

data Parameter
  = Constant T.Text
  | Address LongAddress
  | AtAddress LongAddress
  deriving (Eq, Ord, Show)

fieldAddress :: Field -> LongAddress
fieldAddress (Field address _ _ _) = address

fieldBytes :: Field -> SB.ShortByteString
fieldBytes (Field _ bytes _ _) = bytes

data NextAction
  = Continue
  | Jump !Word16
  | JumpRel !Int8
  | Fork !Word16
  | ForkRel !Int8
  | Stop
  deriving (Eq, Ord, Show)

newtype Disassembly = Disassembly (IM.IntMap Field)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

type Editable = Bool

type Labels = [(LongAddress, (T.Text, Editable))]

-- | Insert a new field into the disassembly. This is somewhat complicated since
-- there may already be data fields that need to be split or truncated to
-- accomodate the new disassembled field.
insert :: Disassembly -> Field -> Disassembly
insert (Disassembly m) field =
  Disassembly
    ( m
        & ( deleteAll obsoleteFields
              >>> insertAll (concat (truncateField <$> obsoleteFields))
              >>> IM.insert
                (key field)
                ( if length overlappingFields /= length obsoleteFields
                    then setOverlapping field
                    else field
                )
          )
    )
  where
    Field address bytes _ _ = field
    nextAddress = address `addOffset` SB.length bytes
    deleteAll = flip (foldl' (\m' f -> IM.delete (key f) m'))
    insertAll = flip (foldl' (\m' f -> IM.insert (key f) f m'))
    key f = let Field a _ _ _ = f in encodeAddress a
    isData (Field _ _ _ Data) = True
    isData _ = False
    bytesChanged (Field a b _ _) =
      let diff = a `minus` address
          (bytesOld, bytesNew) =
            if diff > 0
              then (SB.unpack b, drop diff (SB.unpack bytes))
              else (drop (- diff) (SB.unpack b), SB.unpack bytes)
       in or (zipWith (/=) bytesOld bytesNew)
    isOverlapping (Field fAddress fBytes _ _) =
      let fNextAddress = fAddress `addOffset` SB.length fBytes
       in not (nextAddress <= fAddress || fNextAddress <= address)
    obsoleteFields = filter (\f -> isData f || bytesChanged f) overlappingFields
    overlappingFields =
      let (before, mv, after) = IM.splitLookup (key field) m
          overlappingBefore = takeWhile isOverlapping (snd <$> IM.toDescList before)
          overlappingAfter = takeWhile isOverlapping (snd <$> IM.toAscList after)
       in case mv of
            Nothing -> overlappingBefore ++ overlappingAfter
            Just v -> v : overlappingBefore ++ overlappingAfter

    setOverlapping f = let Field a b _ fieldData = f in Field a b True fieldData
    minus (LongAddress _ offset0) (LongAddress _ offset1) =
      fromIntegral offset0 - fromIntegral offset1
    truncateField f =
      let Field fAddress fBytes o d = f
          leftPart =
            if address > fAddress
              then Just (Field fAddress (unpacked (take (address `minus` fAddress)) fBytes) o d)
              else Nothing
          rightPart =
            if nextAddress < (fAddress `addOffset` SB.length fBytes)
              then Just (Field nextAddress (unpacked (drop (nextAddress `minus` fAddress)) fBytes) o d)
              else Nothing
       in catMaybes [leftPart, rightPart]
    unpacked f = SB.pack . f . SB.unpack

-- | Encode a LongAddress into an Int so that when the address are ordered by
-- that Int, the banks are sorted and group in a sensible way.
encodeAddress :: LongAddress -> Int
encodeAddress (LongAddress bank offset) =
  section .|. (fromIntegral bank .<<. 16) .|. fromIntegral offset
  where
    section =
      if offset < 0x8000
        then if bank == 0xFFFF then 0 else 0x100000000
        else (fromIntegral offset .&. 0xE000) .<<. 19

-- | Get the field at exactly the given address.
lookup :: Disassembly -> LongAddress -> Maybe Field
lookup (Disassembly m) longAddress = IM.lookup (encodeAddress longAddress) m

-- | Get the next n disassembled fields starting at the given address. If there is a
-- field at the given address, then it is included in the output. Otherwise the
-- first field returned is the first field that occurs after the given address.
-- If a negative n is used, then the previous n fields are returned in reverse
-- order (highest to lowest address). Again, if there is a field at the given
-- address then it is included in the output.
lookupN :: Disassembly -> Int -> LongAddress -> [Field]
lookupN (Disassembly m) n startAddress
  | n == 0 =
    []
  | n < 0 =
    let rest = take (- n) leftList
     in case mv of
          Just v -> v : rest
          Nothing -> case rightList of
            [] -> rest
            (v : _) -> v : rest
  | otherwise =
    let rest = take n rightList
     in case mv of
          Just v -> v : rest
          Nothing -> case leftList of
            [] -> rest
            (v : _) -> v : rest
  where
    key = encodeAddress startAddress
    (l, mv, r) = IM.splitLookup key m
    leftList = snd <$> IM.toDescList l
    rightList = snd <$> IM.toAscList r

-- | Generate disassembly.
generateOutput :: Disassembly -> (LongAddress -> Maybe T.Text) -> LT.Text
generateOutput (Disassembly disassembly) lookupLabel =
  TB.toLazyText
    . mconcat
    $ map ((<> "\n") . generateLine)
      . filter (both . bimap inROMRange (not . isOverlapping))
      . map (first decodeAddress)
      $ IM.toList disassembly
  where
    generateLine (address, Field _ bytesString _ instruction) =
      let bytes = SB.unpack bytesString
       in generateHeader address <> generateLabel address <> "  " <> case instruction of
            Data ->
              mconcat ("db " : intersperse ", " (TB.fromString . ('$' :) . formatHex <$> bytes))
            Instruction0 t -> TB.fromText t
            Instruction1 t p1 -> TB.fromText t <> " " <> generateParameter p1
            Instruction2 t p1 p2 ->
              TB.fromText t <> " " <> generateParameter p1 <> ", " <> generateParameter p2
    generateParameter p = case p of
      Constant t -> TB.fromText t
      Address address -> generateAddressParameter address
      AtAddress address -> "(" <> generateAddressParameter address <> ")"
    generateAddressParameter address@(LongAddress _ offset) =
      maybe (TB.fromString ('$' : formatHex offset)) TB.fromText (lookupLabel address)
    generateLabel address = case lookupLabel address of
      Nothing -> ""
      Just label -> case T.uncons label of
        Nothing -> ""
        Just (c1, _) ->
          (if isAlphaNum c1 || c1 == '_' then "\n" else "") <> TB.fromText label <> ":\n"
    generateHeader (LongAddress bank offset) =
      if offset == 0x4000 then "\n Bank $" <> TB.fromString (formatHex bank) <> "\n" else ""
    decodeAddress key =
      LongAddress (fromIntegral ((key .>>. 16) .&. 0xFFFF)) (fromIntegral (key .&. 0xFFFF))
    inROMRange (LongAddress bank offset) = offset < 0x8000 && bank /= 0xFFFF
    isOverlapping (Field _ _ overlapping _) = overlapping
    both (l, r) = l && r

data Banks = Banks
  { bankBootLimit :: !Word16, -- First address that is not in BOOT ROM (excluding hole at 0x100~0x200)
    bank0ROM :: !Word16,
    bank1ROM :: !Word16,
    bankRAM :: !Word16,
    bankWRAM :: !Word16,
    bankVRAM :: !Word16
  }
  deriving (Eq, Show)

data DisassemblyState = DisassemblyState
  { disassemblyPC :: !Word16,
    disassemblyBanks :: !Banks,
    disassemblyBytes :: ![Word8],
    disassemblyMemory :: !Memory.State
  }

-- | Get the current memory bank numbers.
getBanks :: Memory.Has env => ReaderT env IO Banks
getBanks = do
  bankBootLimit <- bootLimit
  bank0ROM <- Memory.getBank 0x3000
  bank1ROM <- Memory.getBank 0x4000
  bankRAM <- Memory.getBank 0xA000
  bankWRAM <- Memory.getBank 0xD000
  bankVRAM <- Memory.getBank 0x8000
  pure Banks {..}
  where
    bootLimit = do
      memory0 <- asks Memory.forState
      isBootEnabled <- Memory.getBank 0x0000 <&> (== 0xFFFF)
      pure (if isBootEnabled then fromIntegral (Memory.bootROMLength memory0) else 0)

-- | Get the current bank associated with an address.
lookupBank :: Word16 -> Banks -> Word16
lookupBank offset Banks {..}
  | offset < bankBootLimit && (offset < 0x100 || offset >= 0x200) = 0xFFFF
  | offset < 0x4000 = bank0ROM
  | offset < 0x8000 = bank1ROM
  | offset < 0xA000 = bankVRAM
  | offset < 0xC000 = bankRAM
  | offset < 0xD000 = 0
  | offset < 0xE000 = bankWRAM
  | offset < 0xF000 = 0
  | offset < 0xFE00 = bankWRAM
  | otherwise = 0

-- | Check if more disassembly is required at the given address.
disassemblyRequired :: Memory.Has env => LongAddress -> Disassembly -> ReaderT env IO Bool
disassemblyRequired address@(LongAddress _ pc) disassembly = case lookup disassembly address of
  Nothing -> pure True
  Just (Field _ bytes _ fieldData) -> case fieldData of
    Data -> pure True
    _ -> do
      actualBytes <- traverse Memory.readByte (take (SB.length bytes) [pc ..])
      pure (or (zipWith (/=) actualBytes (SB.unpack bytes)))

-- | Disassemble the ROM currently loaded in memory.
disassembleROM :: Memory.State -> [LongAddress] -> IO (Disassembly, Labels)
disassembleROM memory0 extraRoots =
  runReaderT
    ( disassembleFromRoots
        (bootSubstrate <> substrate)
        ( (if Memory.hasBootROM memory0 then LongAddress 0xFFFF 0 else LongAddress 0 0x100) :
          ( LongAddress 0
              <$> [0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38, 0x40, 0x48, 0x50, 0x58, 0x60]
          )
            <> extraRoots
        )
    )
    memory0
  where
    bootSubstrate = case Memory.getBootROMData memory0 of
      Nothing -> mempty
      Just bootROM ->
        Disassembly
          (IM.fromList (filter (isAtBootROMAddress . snd) (toData bootROMAddressing bootROM)))
    isAtBootROMAddress (Field (LongAddress bank offset) _ _ _) =
      bank == 0xFFFF && (offset < 0x100 || offset >= 0x200)
    substrate = Disassembly (IM.fromList (toData romAddressing (Memory.getROMData memory0)))
    toData addressing rawData =
      zipWith
        (\address bytes -> (encodeAddress address, Field address (SB.pack bytes) False Data))
        addressing
        $ chunksOf 16 (VS.toList rawData)
    bootROMAddressing = LongAddress 0xFFFF <$> [0, 0x10 .. 0x1000]
    romAddressing =
      (LongAddress 0 <$> [0, 0x10 .. 0x3FFF])
        ++ [LongAddress bank offset | bank <- [1 ..], offset <- [0x4000, 0x4010 .. 0x7FFF]]
    chunksOf n = go
      where
        go [] = []
        go xs = let (r, rs) = splitAt n xs in r : go rs

-- | The defualt set of labels that are relevant to all GBC programs.
initialLabels :: Labels
initialLabels =
  [ (LongAddress 0 0x0, ("rst0", False)),
    (LongAddress 0 0x8, ("rst1", False)),
    (LongAddress 0 0x10, ("rst2", False)),
    (LongAddress 0 0x18, ("rst3", False)),
    (LongAddress 0 0x20, ("rst4", False)),
    (LongAddress 0 0x28, ("rst5", False)),
    (LongAddress 0 0x30, ("rst6", False)),
    (LongAddress 0 0x38, ("rst7", False)),
    (LongAddress 0 0x40, ("int40_vblank", False)),
    (LongAddress 0 0x48, ("int48_lcd", False)),
    (LongAddress 0 0x50, ("int50_timer", False)),
    (LongAddress 0 0x58, ("int58_serial", False)),
    (LongAddress 0 0x60, ("int60_keypad", False)),
    (LongAddress 0 0x100, ("rom_header", False)),
    (LongAddress 0 P1, ("P1", False)),
    (LongAddress 0 SB, ("SB", False)),
    (LongAddress 0 SC, ("SC", False)),
    (LongAddress 0 DIV, ("DIV", False)),
    (LongAddress 0 TIMA, ("TIMA", False)),
    (LongAddress 0 TMA, ("TMA", False)),
    (LongAddress 0 TAC, ("TAC", False)),
    (LongAddress 0 NR10, ("NR10", False)),
    (LongAddress 0 NR11, ("NR11", False)),
    (LongAddress 0 NR12, ("NR12", False)),
    (LongAddress 0 NR13, ("NR13", False)),
    (LongAddress 0 NR14, ("NR14", False)),
    (LongAddress 0 NR20, ("NR20", False)),
    (LongAddress 0 NR21, ("NR21", False)),
    (LongAddress 0 NR22, ("NR22", False)),
    (LongAddress 0 NR23, ("NR23", False)),
    (LongAddress 0 NR24, ("NR24", False)),
    (LongAddress 0 NR30, ("NR30", False)),
    (LongAddress 0 NR31, ("NR31", False)),
    (LongAddress 0 NR32, ("NR32", False)),
    (LongAddress 0 NR33, ("NR33", False)),
    (LongAddress 0 NR34, ("NR34", False)),
    (LongAddress 0 NR40, ("NR40", False)),
    (LongAddress 0 NR41, ("NR41", False)),
    (LongAddress 0 NR42, ("NR42", False)),
    (LongAddress 0 NR43, ("NR43", False)),
    (LongAddress 0 NR44, ("NR44", False)),
    (LongAddress 0 NR50, ("NR50", False)),
    (LongAddress 0 NR51, ("NR51", False)),
    (LongAddress 0 NR52, ("NR52", False)),
    (LongAddress 0 IF, ("IF", False)),
    (LongAddress 0 LCDC, ("LCDC", False)),
    (LongAddress 0 STAT, ("STAT", False)),
    (LongAddress 0 SCY, ("SCY", False)),
    (LongAddress 0 SCX, ("SCX", False)),
    (LongAddress 0 LY, ("LY", False)),
    (LongAddress 0 LYC, ("LYC", False)),
    (LongAddress 0 DMA, ("DMA", False)),
    (LongAddress 0 BGP, ("BGP", False)),
    (LongAddress 0 OBP0, ("OBP0", False)),
    (LongAddress 0 OBP1, ("OBP1", False)),
    (LongAddress 0 WY, ("WY", False)),
    (LongAddress 0 WX, ("WX", False)),
    (LongAddress 0 R4C, ("R4C", False)),
    (LongAddress 0 KEY1, ("KEY1", False)),
    (LongAddress 0 VBK, ("VBK", False)),
    (LongAddress 0 BLCK, ("BLCK", False)),
    (LongAddress 0 HDMA1, ("HDMA1", False)),
    (LongAddress 0 HDMA2, ("HDMA2", False)),
    (LongAddress 0 HDMA3, ("HDMA3", False)),
    (LongAddress 0 HDMA4, ("HDMA4", False)),
    (LongAddress 0 HDMA5, ("HDMA5", False)),
    (LongAddress 0 RP, ("RP", False)),
    (LongAddress 0 BCPS, ("BCPS", False)),
    (LongAddress 0 BCPD, ("BCPD", False)),
    (LongAddress 0 OCPS, ("OCPS", False)),
    (LongAddress 0 OCPD, ("OCPD", False)),
    (LongAddress 0 R6C, ("R6C", False)),
    (LongAddress 0 SVBK, ("SVBK", False)),
    (LongAddress 0 R72, ("R72", False)),
    (LongAddress 0 R73, ("R73", False)),
    (LongAddress 0 R74, ("R74", False)),
    (LongAddress 0 R75, ("R75", False)),
    (LongAddress 0 PCM12, ("PCM12", False)),
    (LongAddress 0 PCM34, ("PCM34", False)),
    (LongAddress 0 IE, ("IE", False))
  ]
    <> [(LongAddress 0 (0xFE00 + i * 4), ("OBJ" <> T.pack (show i), False)) | i <- [0 .. 39]]

-- | Generate disassembly starting at a particular PC.
disassembleFrom :: Memory.Has env => Word16 -> Disassembly -> ReaderT env IO (Disassembly, Labels)
disassembleFrom pc disassembly = do
  memory <- asks Memory.forState
  banks <- getBanks
  liftIO (disassemble (DisassemblyState pc banks [] memory) disassembly)

-- | Disassemble starting from a list of root addresses.
disassembleFromRoots ::
  Memory.Has env => Disassembly -> [LongAddress] -> ReaderT env IO (Disassembly, Labels)
disassembleFromRoots disassembly0 roots = do
  memory <- asks Memory.forState
  liftIO (runStateT (foldM (disassembleS memory) disassembly0 roots) [])
  where
    disassembleS :: Memory.State -> Disassembly -> LongAddress -> StateT Labels IO Disassembly
    disassembleS memory d0 (LongAddress bank pc) = do
      labels <- get
      (r, labels') <- liftIO (disassemble state0 d0)
      put (labels' ++ labels)
      pure r
      where
        state0 =
          DisassemblyState
            pc
            Banks
              { bankBootLimit = if bank == 0xFFFF then fromIntegral (Memory.bootROMLength memory) else 0,
                bank0ROM = if pc < 0x4000 then bank else 0,
                bank1ROM = if pc >= 0x4000 && pc < 0x8000 then bank else 1,
                bankRAM = 1,
                bankWRAM = 1,
                bankVRAM = 1
              }
            []
            memory

-- | Generate disassembly.
disassemble :: DisassemblyState -> Disassembly -> IO (Disassembly, Labels)
disassemble state0 disassembly = evalStateT (go (disassembly, [])) state0
  where
    go (!accum, !labels) = do
      addressLong <- currentAddressLong
      (action, r) <- decodeAndExecute =<< nextByte
      bs <- takeAccumulatedBytes
      let oldDisassembly = lookup accum addressLong
      if noUpdateRequired oldDisassembly bs
        then pure (accum, labels)
        else do
          let accum' = insert accum (Field addressLong bs False r)
          case action of
            Continue -> go (accum', labels)
            Jump nn -> do
              label <- (,) <$> makeLongAddress nn <*> (liftIO nextGlobalLabel <&> (,True))
              modifyPC (const nn)
              go (accum', label : labels)
            JumpRel i -> do
              label <-
                (,)
                  <$> (currentAddressLong <&> (`addOffset` i))
                  <*> (liftIO (nextLocalLabel i) <&> (,True))
              modifyPC (+ fromIntegral i)
              go (accum', label : labels)
            Fork nn -> do
              s <- get
              label <- (,) <$> makeLongAddress nn <*> (liftIO nextGlobalLabel <&> (,True))
              (accum'', labels') <- liftIO (disassemble (s {disassemblyPC = nn}) accum')
              go (accum'', label : labels' ++ labels)
            ForkRel i -> do
              s <- get
              label <-
                (,)
                  <$> (currentAddressLong <&> (`addOffset` i))
                  <*> (liftIO (nextLocalLabel i) <&> (,True))
              (accum'', labels') <-
                liftIO
                  (disassemble (s {disassemblyPC = disassemblyPC s + fromIntegral i}) accum')
              go (accum'', label : labels' ++ labels)
            Stop -> pure (accum', labels)

    noUpdateRequired Nothing _ = False
    noUpdateRequired (Just (Field _ _ _ Data)) _ = False
    noUpdateRequired (Just (Field _ oldBytes _ _)) newBytes = oldBytes == newBytes

currentAddressLong :: StateT DisassemblyState IO LongAddress
currentAddressLong = makeLongAddress =<< gets disassemblyPC

makeLongAddress :: Word16 -> StateT DisassemblyState IO LongAddress
makeLongAddress offset = do
  bank <- lookupBank offset <$> gets disassemblyBanks
  pure (LongAddress bank offset)

-- | Take the disassembled bytes that have been accumulated in the
-- DisassemblyState.
takeAccumulatedBytes :: StateT DisassemblyState IO SB.ShortByteString
takeAccumulatedBytes = do
  s <- get
  put (s {disassemblyBytes = []})
  pure (SB.pack (reverse (disassemblyBytes s)))

-- | Apply a function to the disassembly PC.
modifyPC :: (Word16 -> Word16) -> StateT DisassemblyState IO ()
modifyPC f = do
  s <- get
  put (s {disassemblyPC = f (disassemblyPC s)})

formatR8 :: Register8 -> Parameter
formatR8 RegA = Constant "A"
formatR8 RegB = Constant "B"
formatR8 RegC = Constant "C"
formatR8 RegD = Constant "D"
formatR8 RegE = Constant "E"
formatR8 RegH = Constant "H"
formatR8 RegL = Constant "L"

formatR16 :: Register16 -> Parameter
formatR16 RegBC = Constant "BC"
formatR16 RegDE = Constant "DE"
formatR16 RegHL = Constant "HL"
formatR16 RegSP = Constant "SP"

formatRpp :: RegisterPushPop -> Parameter
formatRpp PushPopAF = Constant "AF"
formatRpp PushPopBC = Constant "BC"
formatRpp PushPopDE = Constant "DE"
formatRpp PushPopHL = Constant "HL"

formatCC :: ConditionCode -> Parameter
formatCC CondC = Constant "C"
formatCC CondNC = Constant "NC"
formatCC CondZ = Constant "Z"
formatCC CondNZ = Constant "NZ"

formatW8 :: Word8 -> Parameter
formatW8 i = Constant (fromString ('$' : formatHex i))

formatB8 :: Word8 -> Parameter
formatB8 i = Constant (fromString (show i))

formatI8 :: Int8 -> Parameter
formatI8 i = Constant (fromString (show i))

formatA8 :: Word8 -> Parameter
formatA8 w = AtAddress (LongAddress 0 (0xFF00 .|. fromIntegral w))

addCurrentPC :: Word16 -> StateT DisassemblyState IO Word16
addCurrentPC address = (address +) <$> gets disassemblyPC

lookupCurrentBank :: Word16 -> StateT DisassemblyState IO Word16
lookupCurrentBank offset = lookupBank offset <$> gets disassemblyBanks

instance MonadFetch (StateT DisassemblyState IO) where
  nextByte = do
    s@DisassemblyState {..} <- get
    let pc = disassemblyPC
    let pc' = pc + 1
    let bank = lookupBank pc disassemblyBanks
    let banks' =
          if pc' == 0x100 && bankBootLimit disassemblyBanks /= 0
            then disassemblyBanks {bankBootLimit = 0}
            else disassemblyBanks
    byte <- liftIO (runReaderT (Memory.readByteLong bank pc) disassemblyMemory)
    put
      ( s
          { disassemblyPC = pc',
            disassemblyBanks = banks',
            disassemblyBytes = byte : disassemblyBytes
          }
      )
    pure byte

instance MonadGMBZ80 (StateT DisassemblyState IO) where
  type ExecuteResult (StateT DisassemblyState IO) = (NextAction, Instruction)
  ldrr r r' = pure (Continue, Instruction2 "LD" (formatR8 r) (formatR8 r'))
  ldrn r n = pure (Continue, Instruction2 "LD" (formatR8 r) (formatW8 n))
  ldrHL r = pure (Continue, Instruction2 "LD" (formatR8 r) (Constant "(HL)"))
  ldHLr r = pure (Continue, Instruction2 "LD" (Constant "(HL)") (formatR8 r))
  ldHLn n = pure (Continue, Instruction2 "LD" (Constant "(HL)") (formatW8 n))
  ldaBC = pure (Continue, Instruction2 "LD" (Constant "A") (Constant "(BC)"))
  ldaDE = pure (Continue, Instruction2 "LD" (Constant "A") (Constant "(DE)"))
  ldaC = pure (Continue, Instruction2 "LD" (Constant "A") (Constant "(C)"))
  ldCa = pure (Continue, Instruction2 "LD" (Constant "(C)") (Constant "A"))
  ldan n = pure (Continue, Instruction2 "LD" (Constant "A") (formatA8 n))
  ldna n = pure (Continue, Instruction2 "LD" (formatA8 n) (Constant "A"))
  ldann nn = do
    bank <- lookupCurrentBank nn
    pure (Continue, Instruction2 "LD" (Constant "A") (AtAddress (LongAddress bank nn)))
  ldnna nn = do
    bank <- lookupCurrentBank nn
    pure (Continue, Instruction2 "LD" (AtAddress (LongAddress bank nn)) (Constant "A"))
  ldaHLI = pure (Continue, Instruction2 "LD" (Constant "A") (Constant "(HLI)"))
  ldaHLD = pure (Continue, Instruction2 "LD" (Constant "A") (Constant "(HLD)"))
  ldBCa = pure (Continue, Instruction2 "LD" (Constant "(BC)") (Constant "A"))
  ldDEa = pure (Continue, Instruction2 "LD" (Constant "(DE)") (Constant "A"))
  ldHLIa = pure (Continue, Instruction2 "LD" (Constant "(HLI)") (Constant "A"))
  ldHLDa = pure (Continue, Instruction2 "LD" (Constant "(HLD)") (Constant "A"))
  ldddnn dd nn = do
    bank <- lookupCurrentBank nn
    pure (Continue, Instruction2 "LD" (formatR16 dd) (Address (LongAddress bank nn)))
  ldSPHL = pure (Continue, Instruction2 "LD" (Constant "SP") (Constant "HL"))
  push qq = pure (Continue, Instruction1 "PUSH" (formatRpp qq))
  pop qq = pure (Continue, Instruction1 "POP" (formatRpp qq))
  ldhl i = pure (Continue, Instruction2 "LDHL" (Constant "SP") (formatI8 i))
  ldnnSP nn = do
    bank <- lookupCurrentBank nn
    pure (Continue, Instruction2 "LD" (AtAddress (LongAddress bank nn)) (Constant "SP"))
  addr r = pure (Continue, Instruction2 "ADD" (Constant "A") (formatR8 r))
  addn w = pure (Continue, Instruction2 "ADD" (Constant "A") (formatW8 w))
  addhl = pure (Continue, Instruction2 "ADD" (Constant "A") (Constant "(HL)"))
  adcr r = pure (Continue, Instruction2 "ADC" (Constant "A") (formatR8 r))
  adcn i = pure (Continue, Instruction2 "ADC" (Constant "A") (formatW8 i))
  adchl = pure (Continue, Instruction2 "ADC" (Constant "A") (Constant "(HL)"))
  subr r = pure (Continue, Instruction2 "SUB" (Constant "A") (formatR8 r))
  subn i = pure (Continue, Instruction2 "SUB" (Constant "A") (formatW8 i))
  subhl = pure (Continue, Instruction2 "SUB" (Constant "A") (Constant "(HL)"))
  sbcr r = pure (Continue, Instruction2 "SBC" (Constant "A") (formatR8 r))
  sbcn i = pure (Continue, Instruction2 "SBC" (Constant "A") (formatW8 i))
  sbchl = pure (Continue, Instruction2 "SBC" (Constant "A") (Constant "(HL)"))
  andr r = pure (Continue, Instruction2 "AND" (Constant "A") (formatR8 r))
  andn i = pure (Continue, Instruction2 "AND" (Constant "A") (formatW8 i))
  andhl = pure (Continue, Instruction2 "AND" (Constant "A") (Constant "(HL)"))
  orr r = pure (Continue, Instruction2 "OR" (Constant "A") (formatR8 r))
  orn i = pure (Continue, Instruction2 "OR" (Constant "A") (formatW8 i))
  orhl = pure (Continue, Instruction2 "OR" (Constant "A") (Constant "(HL)"))
  xorr r = pure (Continue, Instruction2 "XOR" (Constant "A") (formatR8 r))
  xorn i = pure (Continue, Instruction2 "XOR" (Constant "A") (formatW8 i))
  xorhl = pure (Continue, Instruction2 "XOR" (Constant "A") (Constant "(HL)"))
  cpr r = pure (Continue, Instruction2 "CP" (Constant "A") (formatR8 r))
  cpn i = pure (Continue, Instruction2 "CP" (Constant "A") (formatW8 i))
  cphl = pure (Continue, Instruction2 "CP" (Constant "A") (Constant "(HL)"))
  incr r = pure (Continue, Instruction1 "INC" (formatR8 r))
  inchl = pure (Continue, Instruction1 "INC" (Constant "(HL)"))
  decr r = pure (Continue, Instruction1 "DEC" (formatR8 r))
  dechl = pure (Continue, Instruction1 "DEC" (Constant "(HL)"))
  addhlss ss = pure (Continue, Instruction2 "ADD" (Constant "HL") (formatR16 ss))
  addSP i = pure (Continue, Instruction2 "ADD" (Constant "SP") (formatI8 i))
  incss ss = pure (Continue, Instruction1 "INC" (formatR16 ss))
  decss ss = pure (Continue, Instruction1 "DEC" (formatR16 ss))
  rlca = pure (Continue, Instruction0 "RLCA")
  rla = pure (Continue, Instruction0 "RLA")
  rrca = pure (Continue, Instruction0 "RRCA")
  rra = pure (Continue, Instruction0 "RRA")
  rlcr r = pure (Continue, Instruction1 "RLC" (formatR8 r))
  rlchl = pure (Continue, Instruction1 "RLC" (Constant "(HL)"))
  rlr r = pure (Continue, Instruction1 "RL " (formatR8 r))
  rlhl = pure (Continue, Instruction1 "RL " (Constant "(HL)"))
  rrcr r = pure (Continue, Instruction1 "RRC" (formatR8 r))
  rrchl = pure (Continue, Instruction1 "RRC" (Constant "(HL)"))
  rrr r = pure (Continue, Instruction1 "RR " (formatR8 r))
  rrhl = pure (Continue, Instruction1 "RR " (Constant "(HL)"))
  slar r = pure (Continue, Instruction1 "SLA" (formatR8 r))
  slahl = pure (Continue, Instruction1 "SLA" (Constant "(HL)"))
  srar r = pure (Continue, Instruction1 "SRA" (formatR8 r))
  srahl = pure (Continue, Instruction1 "SRA" (Constant "(HL)"))
  srlr r = pure (Continue, Instruction1 "SRL" (formatR8 r))
  srlhl = pure (Continue, Instruction1 "SRL" (Constant "(HL)"))
  swapr r = pure (Continue, Instruction1 "SWAP" (formatR8 r))
  swaphl = pure (Continue, Instruction1 "SWAP" (Constant "(HL)"))
  bitr r i = pure (Continue, Instruction2 "BIT" (formatB8 i) (formatR8 r))
  bithl i = pure (Continue, Instruction2 "BIT" (formatB8 i) (Constant "(HL)"))
  setr r i = pure (Continue, Instruction2 "SET" (formatB8 i) (formatR8 r))
  sethl i = pure (Continue, Instruction2 "SET" (formatB8 i) (Constant "(HL)"))
  resr r i = pure (Continue, Instruction2 "RES" (formatB8 i) (formatR8 r))
  reshl i = pure (Continue, Instruction2 "RES" (formatB8 i) (Constant "(HL)"))
  jpnn nn = do
    bank <- lookupCurrentBank nn
    pure (Jump nn, Instruction1 "JP" (Address (LongAddress bank nn)))
  jphl = pure (Stop, Instruction1 "JP" (Constant "(HL)"))
  jpccnn cc nn = do
    bank <- lookupCurrentBank nn
    pure (Fork nn, Instruction2 "JP" (formatCC cc) (Address (LongAddress bank nn)))
  jr i = do
    offset <- addCurrentPC (fromIntegral i)
    bank <- lookupCurrentBank offset
    pure (JumpRel i, Instruction1 "JR" (Address (LongAddress bank offset)))
  jrcc cc i = do
    offset <- addCurrentPC (fromIntegral i)
    bank <- lookupCurrentBank offset
    pure (ForkRel i, Instruction2 "JR" (formatCC cc) (Address (LongAddress bank offset)))
  call nn = do
    bank <- lookupCurrentBank nn
    pure (Fork nn, Instruction1 "CALL" (Address (LongAddress bank nn)))
  callcc cc nn = do
    bank <- lookupCurrentBank nn
    pure (Fork nn, Instruction2 "CALL" (formatCC cc) (Address (LongAddress bank nn)))
  ret = pure (Stop, Instruction0 "RET")
  reti = pure (Stop, Instruction0 "RETI")
  retcc cc = pure (Continue, Instruction1 "RET " (formatCC cc))
  rst i = pure (Jump (8 * fromIntegral i), Instruction1 "RST " (formatB8 i))
  daa = pure (Continue, Instruction0 "DAA")
  cpl = pure (Continue, Instruction0 "CPL")
  nop = pure (Continue, Instruction0 "NOP")
  ccf = pure (Continue, Instruction0 "CCF")
  scf = pure (Continue, Instruction0 "SCF")
  di = pure (Continue, Instruction0 "DI")
  ei = pure (Continue, Instruction0 "EI")
  halt = pure (Continue, Instruction0 "HALT")
  stop = pure (Continue, Instruction0 "STOP")
  invalid b = pure (Stop, Instruction1 "INVALID" (formatW8 b))
