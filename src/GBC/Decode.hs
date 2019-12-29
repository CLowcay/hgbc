{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GBC.Decode
  ( Decode
  , runDecode
  , decode
  , decodeN
  )
where

import           Common
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Array
import           Data.Bits
import           Data.Word
import           GBC.ISA
import           GBC.Memory

type Decode a = StateT Word16 (ReaderT Memory IO) a

{-# INLINE runDecode #-}
runDecode :: HasMemory env => Word16 -> Decode a -> ReaderT env IO (a, Word16)
runDecode addr computation = do
  mem <- asks forMemory
  liftIO (runReaderT (runStateT computation addr) mem)

table :: Array Word8 (Decode Instruction)
table = array (0, 0xFF) (doDecode <$> [0 .. 0xFF])
 where
  doDecode x =
    (x, decodeBytes (x `unsafeShiftR` 6 .&. 0x03) (x `unsafeShiftR` 3 .&. 0x07) (x .&. 0x07))
  decodeBytes 0 0 0 = pure NOP
  decodeBytes 0 1 0 = LDI16I_SP <$> nextWord
  decodeBytes 0 2 0 = do
    b1 <- nextByte
    pure (if b1 == 0 then STOP else INVALID 0o020)
  decodeBytes 0 3  0 = JR . fromIntegral <$> nextByte
  decodeBytes 0 cc 0 = JRCC (conditionCode (cc .&. 0x03)) . fromIntegral <$> nextByte
  decodeBytes 0 dd 1 = if dd .&. 0x01 == 0
    then LD16_I16 (registerPair dd) <$> nextWord
    else pure (ADDHL (registerPair (dd .&. 0x06)))
  decodeBytes 0 0  2 = pure LDBCI_A
  decodeBytes 0 1  2 = pure LDA_BCI
  decodeBytes 0 2  2 = pure LDDEI_A
  decodeBytes 0 3  2 = pure LDA_DEI
  decodeBytes 0 4  2 = pure LDHLI_INC
  decodeBytes 0 5  2 = pure LDA_INC
  decodeBytes 0 6  2 = pure LDHLI_DEC
  decodeBytes 0 7  2 = pure LDA_DEC
  decodeBytes 0 ss 3 = pure
    $ if ss .&. 0x01 == 0 then INC16 (registerPair ss) else DEC16 (registerPair (ss .&. 0x06))
  decodeBytes 0 6  4  = pure (INC SmallHLI)
  decodeBytes 0 r  4  = pure (INC (SmallR8 (register r)))
  decodeBytes 0 6  5  = pure (DEC SmallHLI)
  decodeBytes 0 r  5  = pure (DEC (SmallR8 (register r)))
  decodeBytes 0 6  6  = LDHLI_I8 <$> nextByte
  decodeBytes 0 r  6  = LD_R8 (register r) . I8 <$> nextByte
  decodeBytes 0 0  7  = pure RLCA
  decodeBytes 0 1  7  = pure RRCA
  decodeBytes 0 2  7  = pure RLA
  decodeBytes 0 3  7  = pure RRA
  decodeBytes 0 4  7  = pure DAA
  decodeBytes 0 5  7  = pure CPL
  decodeBytes 0 6  7  = pure SCF
  decodeBytes 0 7  7  = pure CCF

  decodeBytes 1 6  6  = pure HALT
  decodeBytes 1 r  6  = pure (LD_R8 (register r) HLI)
  decodeBytes 1 6  r  = pure (LDHLI_R8 (register r))
  decodeBytes 1 r1 r2 = pure (LD_R8 (register r1) (R8 (register r2)))

  decodeBytes 2 op 6  = pure (aluOp op HLI)
  decodeBytes 2 op r  = pure (aluOp op (R8 (register r)))

  decodeBytes 3 4  0  = LDI8I_A <$> nextByte
  decodeBytes 3 5  0  = ADDSP . fromIntegral <$> nextByte
  decodeBytes 3 6  0  = LDA_I8I <$> nextByte
  decodeBytes 3 7  0  = LDHL . fromIntegral <$> nextByte
  decodeBytes 3 cc 0  = pure (RETCC (conditionCode cc))
  decodeBytes 3 1  1  = pure RET
  decodeBytes 3 3  1  = pure RETI
  decodeBytes 3 5  1  = pure JPI
  decodeBytes 3 7  1  = pure LDSP
  decodeBytes 3 qq 1  = pure (POP (registerPair qq))
  decodeBytes 3 4  2  = pure LDCI_A
  decodeBytes 3 5  2  = LDI16I_A <$> nextWord
  decodeBytes 3 6  2  = pure LDA_CI
  decodeBytes 3 7  2  = LDA_I16I <$> nextWord
  decodeBytes 3 cc 2  = JPCC (conditionCode cc) <$> nextWord
  decodeBytes 3 0  3  = JP <$> nextWord
  decodeBytes 3 6  3  = pure DI
  decodeBytes 3 7  3  = pure EI
  decodeBytes 3 cc 4  = if cc .&. 0x04 /= 0
    then pure (INVALID (0o304 .|. cc .<<. 3))
    else CALLCC (conditionCode cc) <$> nextWord
  decodeBytes 3 1  5 = CALL <$> nextWord
  decodeBytes 3 3  5 = pure (INVALID 0o335)
  decodeBytes 3 5  5 = pure (INVALID 0o355)
  decodeBytes 3 7  5 = pure (INVALID 0o375)
  decodeBytes 3 qq 5 = pure (PUSH (registerPair qq))
  decodeBytes 3 op 6 = aluOp op . I8 <$> nextByte
  decodeBytes 3 t  7 = pure (RST t)

  decodeBytes 3 1  3 = do
    b1 <- nextByte
    pure $ case splitByte b1 of
      (0, 0, 6) -> RLC SmallHLI
      (0, 0, r) -> RLC (SmallR8 (register r))
      (0, 1, 6) -> RRC SmallHLI
      (0, 1, r) -> RRC (SmallR8 (register r))
      (0, 2, 6) -> RL SmallHLI
      (0, 2, r) -> RL (SmallR8 (register r))
      (0, 3, 6) -> RR SmallHLI
      (0, 3, r) -> RR (SmallR8 (register r))
      (0, 4, 6) -> SLA SmallHLI
      (0, 4, r) -> SLA (SmallR8 (register r))
      (0, 5, 6) -> SRA SmallHLI
      (0, 5, r) -> SRA (SmallR8 (register r))
      (0, 6, 6) -> SWAP SmallHLI
      (0, 6, r) -> SWAP (SmallR8 (register r))
      (0, 7, 6) -> SRL SmallHLI
      (0, 7, r) -> SRL (SmallR8 (register r))
      (1, b, 6) -> BIT b SmallHLI
      (1, b, r) -> BIT b (SmallR8 (register r))
      (2, b, 6) -> RES b SmallHLI
      (2, b, r) -> RES b (SmallR8 (register r))
      (3, b, 6) -> SET b SmallHLI
      (3, b, r) -> SET b (SmallR8 (register r))
      _         -> INVALID b1

  decodeBytes a b c = pure (INVALID ((a .<<. 6) .|. (b .<<. 3) .|. c))

  splitByte x = (x `unsafeShiftR` 6 .&. 0x03, x `unsafeShiftR` 3 .&. 0x07, x .&. 0x07)
  register 0o0 = RegB
  register 0o1 = RegC
  register 0o2 = RegD
  register 0o3 = RegE
  register 0o4 = RegH
  register 0o5 = RegL
  register 0o7 = RegA
  register r   = error ("invalid register code " ++ show r)
  registerPair 0 = RegBC
  registerPair 2 = RegDE
  registerPair 4 = RegHL
  registerPair 6 = RegSP
  registerPair x = error ("invalid register pair code " ++ show x)
  conditionCode 0 = CondNZ
  conditionCode 1 = CondZ
  conditionCode 2 = CondNC
  conditionCode 3 = CondC
  conditionCode x = error ("invalid condition code " ++ show x)
  aluOp 0 = ADD
  aluOp 1 = ADC
  aluOp 2 = SUB
  aluOp 3 = SBC
  aluOp 4 = AND
  aluOp 5 = XOR
  aluOp 6 = OR
  aluOp 7 = CP
  aluOp x = error ("invalid ALU operation code " ++ show x)

{-# INLINABLE decodeN #-}
decodeN :: HasMemory env => Word16 -> Int -> ReaderT env IO [(Word16, Instruction)]
decodeN base = fmap fst . runDecode base . doDecode
 where
  doDecode :: Int -> Decode [(Word16, Instruction)]
  doDecode 0   = pure []
  doDecode len = do
    location <- get
    decode >>= \case
      INVALID _   -> pure []
      instruction -> ((location, instruction) :) <$> doDecode (len - 1)

decode :: Decode Instruction
decode = do
  b0 <- nextByte
  table ! b0

nextByte :: Decode Word8
nextByte = do
  addr <- get
  r    <- lift (readByte addr)
  modify (+ 1)
  pure r

nextWord :: Decode Word16
nextWord = do
  addr <- get
  l    <- lift (readByte addr)
  h    <- lift (readByte (addr + 1))
  modify (+ 2)
  pure $ (fromIntegral h .<<. 8) .|. fromIntegral l
