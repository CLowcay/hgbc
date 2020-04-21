{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Disassembler
  ( LongAddress(..)
  , Field(..)
  , Editable
  , Labels
  , fieldAddress
  , fieldBytes
  , Disassembly
  , DisassemblyState(..)
  , lookupN
  , disassemblyRequired
  , disassembleROM
  , disassembleFrom
  , disassemble
  )
where

import           Control.Category        hiding ( (.) )
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Bits
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Hashable
import           Data.Int
import           Data.Maybe
import           Data.String
import           Data.Word
import           Debugger.LabelGenerator
import           Machine.GBC.CPU.Decode
import           Machine.GBC.CPU.ISA
import           Machine.GBC.Memory
import           Machine.GBC.Registers
import           Machine.GBC.Util
import           Prelude                 hiding ( lookup )
import qualified Data.ByteString.Short         as SB
import qualified Data.IntMap.Lazy              as IM
import qualified Data.Text                     as T
import qualified Data.Vector.Storable          as VS

data LongAddress
  = LongAddress !Word16 !Word16
  deriving (Eq, Ord, Show)

instance Hashable LongAddress where
  hashWithSalt salt (LongAddress bank offset) =
    hashWithSalt salt ((fromIntegral bank .<<. 16) .|. fromIntegral offset :: Int)

instance ToJSON LongAddress where
  toJSON (LongAddress bank address) = object ["offset" .= address, "bank" .= bank]

addOffset :: Integral a => LongAddress -> a -> LongAddress
addOffset (LongAddress bank offset0) offset = LongAddress bank (offset0 + fromIntegral offset)

type Overlap = Bool
data Field =
  Field LongAddress SB.ShortByteString Overlap Instruction
  deriving (Eq, Ord, Show)

data Instruction
  = Data
  | Instruction0 T.Text
  | Instruction1 T.Text Parameter
  | Instruction2 T.Text Parameter Parameter
  deriving (Eq, Ord, Show)
data Parameter
  = Constant T.Text
  | Address Word16
  | AtAddress Word16
  deriving (Eq, Ord, Show)

instance ToJSON Parameter where
  toJSON (Constant text   ) = object ["text" .= text]
  toJSON (Address  address) = object ["text" .= ('$' : formatHex address), "address" .= address]
  toJSON (AtAddress address) =
    object ["text" .= ("($" ++ formatHex address ++ ")"), "address" .= address, "indirect" .= True]

instance ToJSON Field where
  toJSON (Field address bytes overlap fdata) =
    object
      $ ("address" .= address)
      : ("bytes" .= unwords (formatHex <$> SB.unpack bytes))
      : ("overlap" .= overlap)
      : case fdata of
          Data                    -> ["text" .= ("db" :: T.Text)]
          Instruction0 text       -> ["text" .= text, "p" .= ([] :: [String])]
          Instruction1 text p1    -> ["text" .= text, "p" .= [p1]]
          Instruction2 text p1 p2 -> ["text" .= text, "p" .= [p1, p2]]

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
insert (Disassembly m) field = Disassembly
  ( m
  & (   deleteAll overlappingDataFields
    >>> insertAll (concat (truncateField <$> overlappingDataFields))
    >>> IM.insert (key field)
                  (if not (all isData overlappingFields) then setOverlapping field else field)
    )
  )
 where
  Field address bytes _ _ = field
  nextAddress             = address `addOffset` SB.length bytes
  deleteAll               = flip (foldl' (\m' f -> IM.delete (key f) m'))
  insertAll               = flip (foldl' (\m' f -> IM.insert (key f) f m'))
  key f = let Field a _ _ _ = f in encodeAddress a
  isData (Field _ _ _ Data) = True
  isData _                  = False
  isOverlapping (Field fAddress fBytes _ _) =
    let fNextAddress = fAddress `addOffset` SB.length fBytes
    in  not (nextAddress <= fAddress || fNextAddress <= address)
  overlappingDataFields = filter isData overlappingFields
  overlappingFields =
    let (before, mv, after) = IM.splitLookup (key field) m
        overlappingBefore   = takeWhile isOverlapping (snd <$> IM.toDescList before)
        overlappingAfter    = takeWhile isOverlapping (snd <$> IM.toAscList after)
    in  case mv of
          Nothing -> overlappingBefore ++ overlappingAfter
          Just v  -> v : overlappingBefore ++ overlappingAfter

  setOverlapping f = let Field a b _ fieldData = f in Field a b True fieldData
  minus (LongAddress _ offset0) (LongAddress _ offset1) =
    fromIntegral offset0 - fromIntegral offset1
  truncateField f =
    let
      Field fAddress fBytes o d = f
      leftPart                  = if address > fAddress
        then Just (Field fAddress (unpacked (take (address `minus` fAddress)) fBytes) o d)
        else Nothing
      rightPart = if nextAddress < (fAddress `addOffset` SB.length fBytes)
        then Just (Field nextAddress (unpacked (drop (nextAddress `minus` fAddress)) fBytes) o d)
        else Nothing
    in
      catMaybes [leftPart, rightPart]
  unpacked f = SB.pack . f . SB.unpack

-- | Encode a LongAddress into an Int so that when the address are ordered by
-- that Int, the banks are sorted and group in a sensible way.
encodeAddress :: LongAddress -> Int
encodeAddress (LongAddress bank offset) =
  section .|. (fromIntegral bank .<<. 16) .|. fromIntegral offset
 where
  section = if offset < 0x8000
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
  | n == 0
  = []
  | n < 0
  = let rest = take (-n) leftList
    in  case mv of
          Just v  -> v : rest
          Nothing -> case rightList of
            []      -> rest
            (v : _) -> v : rest
  | otherwise
  = let rest = take n rightList
    in  case mv of
          Just v  -> v : rest
          Nothing -> case leftList of
            []      -> rest
            (v : _) -> v : rest
 where
  key        = encodeAddress startAddress
  (l, mv, r) = IM.splitLookup key m
  leftList   = snd <$> IM.toDescList l
  rightList  = snd <$> IM.toAscList r

data DisassemblyState = DisassemblyState {
    disassemblyPC          :: !Word16
  , disassemblyBootLockout :: !Bool
  , disassemblyBytes       :: ![Word8]
  , disassemblyMemory      :: !Memory
}

-- | Check if more disassembly is required at the given address.
disassemblyRequired :: HasMemory env => LongAddress -> Disassembly -> ReaderT env IO Bool
disassemblyRequired address@(LongAddress _ pc) disassembly = case lookup disassembly address of
  Nothing                          -> pure True
  Just (Field _ bytes _ fieldData) -> case fieldData of
    Data -> pure True
    _    -> do
      actualBytes <- traverse readByte (take (SB.length bytes) [pc ..])
      pure (or (zipWith (/=) actualBytes (SB.unpack bytes)))

-- | Disassemble the ROM currently loaded in memory.
disassembleROM :: Memory -> IO (Disassembly, Labels)
disassembleROM memory0 =
  flip runStateT defaultLabels
    $   disassembleS entryPoint (bootSubstrate <> substrate)
    >>= disassembleS (romFrom 0x00)
    >>= disassembleS (romFrom 0x08)
    >>= disassembleS (romFrom 0x10)
    >>= disassembleS (romFrom 0x18)
    >>= disassembleS (romFrom 0x20)
    >>= disassembleS (romFrom 0x28)
    >>= disassembleS (romFrom 0x30)
    >>= disassembleS (romFrom 0x38)
    >>= disassembleS (romFrom 0x40)
    >>= disassembleS (romFrom 0x48)
    >>= disassembleS (romFrom 0x50)
    >>= disassembleS (romFrom 0x58)
    >>= disassembleS (romFrom 0x60)
 where
  disassembleS :: DisassemblyState -> Disassembly -> StateT Labels IO Disassembly
  disassembleS state0 d0 = do
    labels       <- get
    (r, labels') <- liftIO (disassemble state0 d0)
    put (labels' ++ labels)
    pure r

  defaultLabels =
    [ (LongAddress 0 0x0  , ("rst0", False))
      , (LongAddress 0 0x8  , ("rst1", False))
      , (LongAddress 0 0x10 , ("rst2", False))
      , (LongAddress 0 0x18 , ("rst3", False))
      , (LongAddress 0 0x20 , ("rst4", False))
      , (LongAddress 0 0x28 , ("rst5", False))
      , (LongAddress 0 0x30 , ("rst6", False))
      , (LongAddress 0 0x38 , ("rst7", False))
      , (LongAddress 0 0x40 , ("int40_vblank", False))
      , (LongAddress 0 0x48 , ("int48_lcd", False))
      , (LongAddress 0 0x50 , ("int50_timer", False))
      , (LongAddress 0 0x58 , ("int58_serial", False))
      , (LongAddress 0 0x60 , ("int60_keypad", False))
      , (LongAddress 0 0x100, ("rom_header", False))
      , (LongAddress 0 P1   , ("P1", False))
      , (LongAddress 0 SB   , ("SB", False))
      , (LongAddress 0 SC   , ("SC", False))
      , (LongAddress 0 DIV  , ("DIV", False))
      , (LongAddress 0 TIMA , ("TIMA", False))
      , (LongAddress 0 TMA  , ("TMA", False))
      , (LongAddress 0 TAC  , ("TAC", False))
      , (LongAddress 0 NR10 , ("NR10", False))
      , (LongAddress 0 NR11 , ("NR11", False))
      , (LongAddress 0 NR12 , ("NR12", False))
      , (LongAddress 0 NR13 , ("NR13", False))
      , (LongAddress 0 NR14 , ("NR14", False))
      , (LongAddress 0 NR20 , ("NR20", False))
      , (LongAddress 0 NR21 , ("NR21", False))
      , (LongAddress 0 NR22 , ("NR22", False))
      , (LongAddress 0 NR23 , ("NR23", False))
      , (LongAddress 0 NR24 , ("NR24", False))
      , (LongAddress 0 NR30 , ("NR30", False))
      , (LongAddress 0 NR31 , ("NR31", False))
      , (LongAddress 0 NR32 , ("NR32", False))
      , (LongAddress 0 NR33 , ("NR33", False))
      , (LongAddress 0 NR34 , ("NR34", False))
      , (LongAddress 0 NR40 , ("NR40", False))
      , (LongAddress 0 NR41 , ("NR41", False))
      , (LongAddress 0 NR42 , ("NR42", False))
      , (LongAddress 0 NR43 , ("NR43", False))
      , (LongAddress 0 NR44 , ("NR44", False))
      , (LongAddress 0 NR50 , ("NR50", False))
      , (LongAddress 0 NR51 , ("NR51", False))
      , (LongAddress 0 NR52 , ("NR52", False))
      , (LongAddress 0 IF   , ("IF", False))
      , (LongAddress 0 LCDC , ("LCDC", False))
      , (LongAddress 0 STAT , ("STAT", False))
      , (LongAddress 0 SCY  , ("SCY", False))
      , (LongAddress 0 SCX  , ("SCX", False))
      , (LongAddress 0 LY   , ("LY", False))
      , (LongAddress 0 LYC  , ("LYC", False))
      , (LongAddress 0 DMA  , ("DMA", False))
      , (LongAddress 0 BGP  , ("BGP", False))
      , (LongAddress 0 OBP0 , ("OBP0", False))
      , (LongAddress 0 OBP1 , ("OBP1", False))
      , (LongAddress 0 WY   , ("WY", False))
      , (LongAddress 0 WX   , ("WX", False))
      , (LongAddress 0 R4C  , ("R4C", False))
      , (LongAddress 0 KEY1 , ("KEY1", False))
      , (LongAddress 0 VBK  , ("VBK", False))
      , (LongAddress 0 BLCK , ("BLCK", False))
      , (LongAddress 0 HDMA1, ("HDMA1", False))
      , (LongAddress 0 HDMA2, ("HDMA2", False))
      , (LongAddress 0 HDMA3, ("HDMA3", False))
      , (LongAddress 0 HDMA4, ("HDMA4", False))
      , (LongAddress 0 HDMA5, ("HDMA5", False))
      , (LongAddress 0 RP   , ("RP", False))
      , (LongAddress 0 BCPS , ("BCPS", False))
      , (LongAddress 0 BCPD , ("BCPD", False))
      , (LongAddress 0 OCPS , ("OCPS", False))
      , (LongAddress 0 OCPD , ("OCPD", False))
      , (LongAddress 0 R6C  , ("R6C", False))
      , (LongAddress 0 SVBK , ("SVBK", False))
      , (LongAddress 0 R72  , ("R72", False))
      , (LongAddress 0 R73  , ("R73", False))
      , (LongAddress 0 R74  , ("R74", False))
      , (LongAddress 0 R75  , ("R75", False))
      , (LongAddress 0 PCM12, ("PCM12", False))
      , (LongAddress 0 PCM34, ("PCM34", False))
      , (LongAddress 0 IE   , ("IE", False))
      ]
      <> [ (LongAddress 0 (0xFE00 + i * 4), ("OBJ" <> T.pack (show i), False)) | i <- [0 .. 39] ]

  romFrom origin = DisassemblyState origin True [] memory0
  entryPoint    = DisassemblyState (if hasBootROM memory0 then 0 else 0x100) False [] memory0
  bootSubstrate = case getBootROMData memory0 of
    Nothing      -> mempty
    Just bootROM -> Disassembly
      (IM.fromList (filter (isAtBootROMAddress . snd) (toData bootROMAddressing bootROM)))
  isAtBootROMAddress (Field (LongAddress bank offset) _ _ _) =
    bank == 0xFFFF && (offset < 0x100 || offset >= 0x200)
  substrate = Disassembly (IM.fromList (toData romAddressing (getROMData memory0)))
  toData addressing rawData =
    zipWith (\address bytes -> (encodeAddress address, Field address (SB.pack bytes) False Data))
            addressing
      $ chunksOf 16 (VS.toList rawData)
  bootROMAddressing = LongAddress 0xFFFF <$> [0, 0x10 .. 0x1000]
  romAddressing =
    (LongAddress 0 <$> [0, 0x10 .. 0x3FFF])
      ++ [ LongAddress bank offset | bank <- [1 ..], offset <- [0x4000, 0x4010 .. 0x7FFF] ]
  chunksOf n = go
   where
    go [] = []
    go xs = let (r, rs) = splitAt n xs in r : go rs

-- | Generate disassembly starting at a particular PC.
disassembleFrom :: HasMemory env => Word16 -> Disassembly -> ReaderT env IO (Disassembly, Labels)
disassembleFrom pc disassembly = do
  memory <- asks forMemory
  liftIO (disassemble (DisassemblyState pc False [] memory) disassembly)

-- | Generate disassembly.
disassemble :: DisassemblyState -> Disassembly -> IO (Disassembly, Labels)
disassemble state0 disassembly = evalStateT (go (disassembly, [])) state0
 where
  go (!accum, !labels) = do
    addressLong <- currentAddressLong
    (action, r) <- fetchAndExecute
    bs          <- takeAccumulatedBytes
    let oldDisassembly = lookup accum addressLong
    if noUpdateRequired oldDisassembly bs
      then pure (accum, labels)
      else do
        let accum' = insert accum (Field addressLong bs False r)
        case action of
          Continue -> go (accum', labels)
          Jump nn  -> do
            label <- (,) <$> makeLongAddress nn <*> (liftIO nextGlobalLabel <&> (, True))
            modifyPC (const nn)
            go (accum', label : labels)
          JumpRel i -> do
            label <-
              (,)
              <$> (currentAddressLong <&> (`addOffset` i))
              <*> (liftIO (nextLocalLabel i) <&> (, True))
            modifyPC (+ fromIntegral i)
            go (accum', label : labels)
          Fork nn -> do
            s <- get
            label <- (,) <$> makeLongAddress nn <*> (liftIO nextGlobalLabel <&> (, True))
            (accum'', labels') <- liftIO (disassemble (s { disassemblyPC = nn }) accum')
            go (accum'', label : labels' ++ labels)
          ForkRel i -> do
            s     <- get
            label <-
              (,)
              <$> (currentAddressLong <&> (`addOffset` i))
              <*> (liftIO (nextLocalLabel i) <&> (, True))
            (accum'', labels') <- liftIO
              (disassemble (s { disassemblyPC = disassemblyPC s + fromIntegral i }) accum')
            go (accum'', label : labels' ++ labels)
          Stop -> pure (accum', labels)

  noUpdateRequired Nothing                          _        = False
  noUpdateRequired (Just (Field _ _        _ Data)) _        = False
  noUpdateRequired (Just (Field _ oldBytes _ _   )) newBytes = oldBytes == newBytes

currentAddressLong :: StateT DisassemblyState IO LongAddress
currentAddressLong = makeLongAddress =<< gets disassemblyPC

makeLongAddress :: Word16 -> StateT DisassemblyState IO LongAddress
makeLongAddress offset = do
  s    <- get
  bank <- if disassemblyBootLockout s && offset < 0x1000
    then pure 0
    else liftIO (runReaderT (getBank offset) (disassemblyMemory s))
  pure (LongAddress bank offset)

-- | Take the disassembled bytes that have been accumulated in the
-- DisassemblyState.
takeAccumulatedBytes :: StateT DisassemblyState IO SB.ShortByteString
takeAccumulatedBytes = do
  s <- get
  put (s { disassemblyBytes = [] })
  pure (SB.pack (reverse (disassemblyBytes s)))

-- | Apply a function to the disassembly PC.
modifyPC :: (Word16 -> Word16) -> StateT DisassemblyState IO ()
modifyPC f = do
  s <- get
  put (s { disassemblyPC = f (disassemblyPC s) })

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
formatCC CondC  = Constant "C"
formatCC CondNC = Constant "NC"
formatCC CondZ  = Constant "Z"
formatCC CondNZ = Constant "NZ"

formatW8 :: Word8 -> Parameter
formatW8 i = Constant (fromString ('$' : formatHex i))

formatB8 :: Word8 -> Parameter
formatB8 i = Constant (fromString (show i))

formatI8 :: Int8 -> Parameter
formatI8 i = Constant (fromString (show i))

formatA8 :: Word8 -> Parameter
formatA8 w = AtAddress (0xFF00 .|. fromIntegral w)

addCurrentPC :: Word16 -> StateT DisassemblyState IO Word16
addCurrentPC address = (address +) <$> gets disassemblyPC

instance MonadFetch (StateT DisassemblyState IO) where
  nextByte = do
    s <- get
    let pc  = disassemblyPC s
    let pc' = disassemblyPC s + 1
    byte <- liftIO
      (runReaderT
        (if pc < 0x1000 && disassemblyBootLockout s then readByteLong 0 pc else readByte pc)
        (disassemblyMemory s)
      )
    put
      (s { disassemblyPC          = pc'
         , disassemblyBootLockout = disassemblyBootLockout s || pc' == 0x100
         , disassemblyBytes       = byte : disassemblyBytes s
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
  ldaC  = pure (Continue, Instruction2 "LD" (Constant "A") (Constant "(C)"))
  ldCa  = pure (Continue, Instruction2 "LD" (Constant "(C)") (Constant "A"))
  ldan n = pure (Continue, Instruction2 "LD" (Constant "A") (formatA8 n))
  ldna n = pure (Continue, Instruction2 "LD" (formatA8 n) (Constant "A"))
  ldann nn = pure (Continue, Instruction2 "LD" (Constant "A") (AtAddress nn))
  ldnna nn = pure (Continue, Instruction2 "LD" (AtAddress nn) (Constant "A"))
  ldaHLI = pure (Continue, Instruction2 "LD" (Constant "A") (Constant "(HLI)"))
  ldaHLD = pure (Continue, Instruction2 "LD" (Constant "A") (Constant "(HLD)"))
  ldBCa  = pure (Continue, Instruction2 "LD" (Constant "(BC)") (Constant "A"))
  ldDEa  = pure (Continue, Instruction2 "LD" (Constant "(DE)") (Constant "A"))
  ldHLIa = pure (Continue, Instruction2 "LD" (Constant "(HLI)") (Constant "A"))
  ldHLDa = pure (Continue, Instruction2 "LD" (Constant "(HLD)") (Constant "A"))
  ldddnn RegHL nn = pure (Continue, Instruction2 "LD" (Constant "HL") (Address nn))
  ldddnn RegSP nn = pure (Continue, Instruction2 "LD" (Constant "SP") (Address nn))
  ldddnn dd nn =
    pure (Continue, Instruction2 "LD" (formatR16 dd) (Constant (T.pack (formatHex nn))))
  ldSPHL = pure (Continue, Instruction2 "LD" (Constant "SP") (Constant "HL"))
  push qq = pure (Continue, Instruction1 "PUSH" (formatRpp qq))
  pop qq = pure (Continue, Instruction1 "POP" (formatRpp qq))
  ldhl i = pure (Continue, Instruction2 "LDHL" (Constant "SP") (formatI8 i))
  ldnnSP nn = pure (Continue, Instruction2 "LD" (AtAddress nn) (Constant "SP"))
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
  rla  = pure (Continue, Instruction0 "RLA")
  rrca = pure (Continue, Instruction0 "RRCA")
  rra  = pure (Continue, Instruction0 "RRA")
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
  jpnn nn = pure (Jump nn, Instruction1 "JP" (Address nn))
  jphl = pure (Stop, Instruction1 "JP" (Constant "(HL)"))
  jpccnn cc nn = pure (Fork nn, Instruction2 "JP" (formatCC cc) (Address nn))
  jr i = do
    address <- Address <$> addCurrentPC (fromIntegral i)
    pure (JumpRel i, Instruction1 "JR" address)
  jrcc cc i = do
    address <- Address <$> addCurrentPC (fromIntegral i)
    pure (ForkRel i, Instruction2 "JR" (formatCC cc) address)
  call nn = pure (Fork nn, Instruction1 "CALL" (Address nn))
  callcc cc nn = pure (Fork nn, Instruction2 "CALL" (formatCC cc) (Address nn))
  ret  = pure (Stop, Instruction0 "RET")
  reti = pure (Stop, Instruction0 "RETI")
  retcc cc = pure (Continue, Instruction1 "RET " (formatCC cc))
  rst i = pure (Jump (8 * fromIntegral i), Instruction1 "RST " (formatB8 i))
  daa  = pure (Continue, Instruction0 "DAA")
  cpl  = pure (Continue, Instruction0 "CPL")
  nop  = pure (Continue, Instruction0 "NOP")
  ccf  = pure (Continue, Instruction0 "CCF")
  scf  = pure (Continue, Instruction0 "SCF")
  di   = pure (Continue, Instruction0 "DI")
  ei   = pure (Continue, Instruction0 "EI")
  halt = pure (Continue, Instruction0 "HALT")
  stop = pure (Continue, Instruction0 "STOP")
  invalid b = pure (Stop, Instruction1 "INVALID" (formatW8 b))
