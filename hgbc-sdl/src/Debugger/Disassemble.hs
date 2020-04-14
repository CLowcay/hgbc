{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Debugger.Disassemble
  ( Field(..)
  , fieldAddress
  , fieldBytes
  , Disassembly
  , DisassemblyState(..)
  , lookupN
  , disassemblyRequired
  , disassembleROM
  , disassembleFromPC
  , disassembleFrom
  )
where

import           Control.Category        hiding ( (.) )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.Bits
import           Data.ByteString.Short
import           Data.Foldable
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.String
import           Data.Word
import           Debugger.Types
import           Machine.GBC.CPU.Decode
import           Machine.GBC.CPU.ISA
import           Machine.GBC.Memory
import           Machine.GBC.Util
import           Prelude                 hiding ( lookup )
import qualified Data.ByteString.Short         as SB
import qualified Data.IntMap.Lazy              as IM
import qualified Data.Text                     as T
import qualified Data.Vector.Storable          as VS

type Overlap = Bool
data FieldData = Instruction T.Text | Data deriving (Eq, Ord, Show)
data Field =
  Field LongAddress ShortByteString Overlap FieldData
  deriving (Eq, Ord, Show)

fieldAddress :: Field -> LongAddress
fieldAddress (Field address _ _ _) = address

fieldBytes :: Field -> ShortByteString
fieldBytes (Field _ bytes _ _) = bytes

instance ToJSON Field where
  toJSON (Field address bytes overlap fdata) = object
    [ "address" .= address
    , "bytes" .= unwords (formatHex <$> SB.unpack bytes)
    , "overlap" .= overlap
    , "text" .= case fdata of
      Data             -> "db"
      Instruction text -> text
    ]

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
  addOffset (LongAddress bank offset0) offset = LongAddress bank (offset0 + fromIntegral offset)
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

encodeAddress :: LongAddress -> Int
encodeAddress (LongAddress bank offset) =
  section .|. (fromIntegral bank .<<. 16) .|. fromIntegral offset
 where
  section = if offset < 0x8000
    then if bank == 0xFFFF then 0 else 0x100000000
    else (fromIntegral offset .&. 0xE000) .<<. 19

lookup :: Disassembly -> LongAddress -> Maybe Field
lookup (Disassembly m) longAddress = IM.lookup (encodeAddress longAddress) m

lookupN :: Disassembly -> Int -> LongAddress -> [Field]
lookupN (Disassembly m) n startAddress | n == 0    = []
                                       | n < 0     = consV (take (-n) leftList)
                                       | otherwise = consV (take n rightList)
 where
  key        = encodeAddress startAddress
  (l, mv, r) = IM.splitLookup key m
  leftList   = snd <$> IM.toDescList l
  rightList  = snd <$> IM.toAscList r
  consV ls = case mv of
    Nothing -> ls
    Just v  -> v : ls

data DisassemblyState = DisassemblyState {
    disassemblyPC          :: !Word16
  , disassemblyBootLockout :: !Bool
  , disassemblyBytes       :: ![Word8]
  , disassemblyMemory      :: !Memory
}

disassemblyRequired :: HasMemory env => LongAddress -> Disassembly -> ReaderT env IO Bool
disassemblyRequired address@(LongAddress _ pc) disassembly = case lookup disassembly address of
  Nothing                          -> pure True
  Just (Field _ bytes _ fieldData) -> case fieldData of
    Data -> pure True
    _    -> do
      actualBytes <- traverse readByte (take (SB.length bytes) [pc ..])
      pure (or (zipWith (/=) actualBytes (SB.unpack bytes)))

disassembleROM :: Memory -> IO Disassembly
disassembleROM memory0 =
  disassembleFrom entryPoint (bootSubstrate <> substrate)
    >>= disassembleFrom (romFrom 0x00)
    >>= disassembleFrom (romFrom 0x08)
    >>= disassembleFrom (romFrom 0x10)
    >>= disassembleFrom (romFrom 0x18)
    >>= disassembleFrom (romFrom 0x20)
    >>= disassembleFrom (romFrom 0x28)
    >>= disassembleFrom (romFrom 0x30)
    >>= disassembleFrom (romFrom 0x38)
    >>= disassembleFrom (romFrom 0x40)
    >>= disassembleFrom (romFrom 0x48)
    >>= disassembleFrom (romFrom 0x50)
    >>= disassembleFrom (romFrom 0x58)
    >>= disassembleFrom (romFrom 0x60)
 where
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

disassembleFromPC :: HasMemory env => Word16 -> Disassembly -> ReaderT env IO Disassembly
disassembleFromPC pc disassembly = do
  memory <- asks forMemory
  liftIO (disassembleFrom (DisassemblyState pc False [] memory) disassembly)

disassembleFrom :: DisassemblyState -> Disassembly -> IO Disassembly
disassembleFrom state0 disassembly = evalStateT (go disassembly) state0
 where
  go !accum = do
    addressLong <- currentAddressLong
    (action, r) <- fetchAndExecute
    bs          <- currentInstructionBytes
    let oldDisassembly = lookup accum addressLong
    if noUpdateRequired oldDisassembly bs
      then pure accum
      else do
        let accum' = insert accum (Field addressLong bs False (Instruction r))
        case action of
          Continue   -> go accum'
          Jump    nn -> modifyAddress (const nn) >> go accum'
          JumpRel i  -> modifyAddress (+ fromIntegral i) >> go accum'
          Fork    nn -> do
            s <- get
            liftIO (disassembleFrom (s { disassemblyPC = nn }) accum') >>= go
          ForkRel i -> do
            s <- get
            go =<< liftIO
              (disassembleFrom (s { disassemblyPC = disassemblyPC s + fromIntegral i }) accum')
          Stop -> pure accum'

  noUpdateRequired Nothing                          _        = False
  noUpdateRequired (Just (Field _ _        _ Data)) _        = False
  noUpdateRequired (Just (Field _ oldBytes _ _   )) newBytes = oldBytes == newBytes

currentAddressLong :: StateT DisassemblyState IO LongAddress
currentAddressLong = do
  s <- get
  let pc = disassemblyPC s
  bank <- if disassemblyBootLockout s && pc < 0x1000
    then pure 0
    else liftIO (runReaderT (getBank pc) (disassemblyMemory s))
  pure (LongAddress bank pc)

currentInstructionBytes :: StateT DisassemblyState IO SB.ShortByteString
currentInstructionBytes = do
  s <- get
  put (s { disassemblyBytes = [] })
  pure (SB.pack (reverse (disassemblyBytes s)))

modifyAddress :: (Word16 -> Word16) -> StateT DisassemblyState IO ()
modifyAddress f = do
  s <- get
  put (s { disassemblyPC = f (disassemblyPC s) })

formatR8 :: Register8 -> T.Text
formatR8 RegA = "A"
formatR8 RegB = "B"
formatR8 RegC = "C"
formatR8 RegD = "D"
formatR8 RegE = "E"
formatR8 RegH = "H"
formatR8 RegL = "L"

formatR16 :: Register16 -> T.Text
formatR16 RegBC = "BC"
formatR16 RegDE = "DE"
formatR16 RegHL = "HL"
formatR16 RegSP = "SP"

formatRpp :: RegisterPushPop -> T.Text
formatRpp PushPopAF = "AF"
formatRpp PushPopBC = "BC"
formatRpp PushPopDE = "DE"
formatRpp PushPopHL = "HL"

formatCC :: ConditionCode -> T.Text
formatCC CondC  = "C"
formatCC CondNC = "NC"
formatCC CondZ  = "Z"
formatCC CondNZ = "NZ"

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
  type ExecuteResult (StateT DisassemblyState IO) = (NextAction, T.Text)
  ldrr r r' = pure (Continue, "LD " <> formatR8 r <> ", " <> formatR8 r')
  ldrn r n = pure (Continue, "LD " <> formatR8 r <> ", " <> fromString (formatHex n))
  ldrHL r = pure (Continue, "LD " <> formatR8 r <> ", (HL)")
  ldHLr r = pure (Continue, "LD (HL), " <> formatR8 r)
  ldHLn n = pure (Continue, "LD (HL), " <> fromString (formatHex n))
  ldaBC = pure (Continue, "LD A, (BC)")
  ldaDE = pure (Continue, "LD A, (DE)")
  ldaC  = pure (Continue, "LD A, (C)")
  ldCa  = pure (Continue, "LD (C), A")
  ldan n = pure (Continue, "LD A, (FF" <> fromString (formatHex n) <> ")")
  ldna n = pure (Continue, "LD (FF" <> fromString (formatHex n) <> "), A")
  ldann nn = pure (Continue, "LD A, (" <> fromString (formatHex nn) <> ")")
  ldnna nn = pure (Continue, "LD (" <> fromString (formatHex nn) <> "), A")
  ldaHLI = pure (Continue, "LD A, (HLI)")
  ldaHLD = pure (Continue, "LD A, (HLD)")
  ldBCa  = pure (Continue, "LD (BC), A")
  ldDEa  = pure (Continue, "LD (DE), A")
  ldHLIa = pure (Continue, "LD (HLI), A")
  ldHLDa = pure (Continue, "LD (HLD), A")
  ldddnn dd nn = pure (Continue, "LD " <> formatR16 dd <> ", " <> fromString (formatHex nn))
  ldSPHL = pure (Continue, "LD SP, HL")
  push qq = pure (Continue, "PUSH " <> formatRpp qq)
  pop qq = pure (Continue, "POP " <> formatRpp qq)
  ldhl i = pure (Continue, "LDHL SP, " <> fromString (formatHex i))
  ldnnSP nn = pure (Continue, "LD (" <> fromString (formatHex nn) <> "), SP")
  addr r = pure (Continue, "ADD A, " <> formatR8 r)
  addn w = pure (Continue, "ADD A, " <> fromString (formatHex w))
  addhl = pure (Continue, "ADD A, (HL)")
  adcr r = pure (Continue, "ADC A, " <> formatR8 r)
  adcn i = pure (Continue, "ADC A, " <> fromString (formatHex i))
  adchl = pure (Continue, "ADC A, (HL)")
  subr r = pure (Continue, "SUB A, " <> formatR8 r)
  subn i = pure (Continue, "SUB A, " <> fromString (formatHex i))
  subhl = pure (Continue, "SUB A, (HL)")
  sbcr r = pure (Continue, "SBC A, " <> formatR8 r)
  sbcn i = pure (Continue, "SBC A, " <> fromString (formatHex i))
  sbchl = pure (Continue, "SBC A, (HL)")
  andr r = pure (Continue, "AND A, " <> formatR8 r)
  andn i = pure (Continue, "AND A, " <> fromString (formatHex i))
  andhl = pure (Continue, "AND A, (HL)")
  orr r = pure (Continue, "OR A, " <> formatR8 r)
  orn i = pure (Continue, "OR A, " <> fromString (formatHex i))
  orhl = pure (Continue, "OR A, (HL)")
  xorr r = pure (Continue, "XOR A, " <> formatR8 r)
  xorn i = pure (Continue, "XOR A, " <> fromString (formatHex i))
  xorhl = pure (Continue, "XOR A, (HL)")
  cpr r = pure (Continue, "CP A, " <> formatR8 r)
  cpn i = pure (Continue, "CP A, " <> fromString (formatHex i))
  cphl = pure (Continue, "CP A, (HL)")
  incr r = pure (Continue, "INC " <> formatR8 r)
  inchl = pure (Continue, "INC (HL)")
  decr r = pure (Continue, "DEC " <> formatR8 r)
  dechl = pure (Continue, "DEC (HL)")
  addhlss ss = pure (Continue, "ADD HL, " <> formatR16 ss)
  addSP i = pure (Continue, "ADD SP, " <> fromString (formatHex i))
  incss ss = pure (Continue, "INC " <> formatR16 ss)
  decss ss = pure (Continue, "DEC " <> formatR16 ss)
  rlca = pure (Continue, "RLCA")
  rla  = pure (Continue, "RLA")
  rrca = pure (Continue, "RRCA")
  rra  = pure (Continue, "RRA")
  rlcr r = pure (Continue, "RLC " <> formatR8 r)
  rlchl = pure (Continue, "RLC (HL)")
  rlr r = pure (Continue, "RL " <> formatR8 r)
  rlhl = pure (Continue, "RL (HL)")
  rrcr r = pure (Continue, "RRC " <> formatR8 r)
  rrchl = pure (Continue, "RRC (HL)")
  rrr r = pure (Continue, "RR " <> formatR8 r)
  rrhl = pure (Continue, "RR (HL)")
  slar r = pure (Continue, "SLA " <> formatR8 r)
  slahl = pure (Continue, "SLA (HL)")
  srar r = pure (Continue, "SRA " <> formatR8 r)
  srahl = pure (Continue, "SRA (HL)")
  srlr r = pure (Continue, "SRL " <> formatR8 r)
  srlhl = pure (Continue, "SRL (HL)")
  swapr r = pure (Continue, "SWAP " <> formatR8 r)
  swaphl = pure (Continue, "SWAP (HL)")
  bitr r i = pure (Continue, "BIT " <> fromString (show i) <> ", " <> formatR8 r)
  bithl i = pure (Continue, "BIT " <> fromString (show i) <> ", (HL)")
  setr r i = pure (Continue, "SET " <> fromString (show i) <> ", " <> formatR8 r)
  sethl i = pure (Continue, "SET " <> fromString (show i) <> ", (HL)")
  resr r i = pure (Continue, "RES " <> fromString (show i) <> ", " <> formatR8 r)
  reshl i = pure (Continue, "RES " <> fromString (show i) <> ", (HL)")
  jpnn nn = pure (Jump nn, "JP " <> fromString (formatHex nn))
  jphl = pure (Stop, "JP (HL)")
  jpccnn cc nn = pure (Fork nn, "JP " <> formatCC cc <> ", " <> fromString (formatHex nn))
  jr i = pure (JumpRel i, "JR " <> fromString (formatHex i))
  jrcc cc i = pure (ForkRel i, "JR " <> formatCC cc <> ", " <> fromString (formatHex i))
  call nn = pure (Fork nn, "CALL " <> fromString (formatHex nn))
  callcc cc nn = pure (Fork nn, "CALL " <> formatCC cc <> ", " <> fromString (formatHex nn))
  ret  = pure (Stop, "RET")
  reti = pure (Stop, "RETI")
  retcc cc = pure (Continue, "RET " <> formatCC cc)
  rst i = pure (Jump (8 * fromIntegral i), "RST " <> fromString (show i))
  daa  = pure (Continue, "DAA")
  cpl  = pure (Continue, "CPL")
  nop  = pure (Continue, "NOP")
  ccf  = pure (Continue, "CCF")
  scf  = pure (Continue, "SCF")
  di   = pure (Continue, "DI")
  ei   = pure (Continue, "EI")
  halt = pure (Continue, "HALT")
  stop = pure (Continue, "STOP")
  invalid b = pure (Stop, "db" <> fromString (formatHex b))
