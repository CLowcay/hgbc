{-# LANGUAGE LambdaCase #-}

module GBC.Disassembler where

import           GBC.Memory
import           GBC.ISA
import           Data.Bits
import           Data.Array
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Word

type Disassembler a = ReaderT Memory (StateT Word16 IO) a

table :: Array Word8 (Disassembler (Maybe Instruction))
table = array (0, 0xFF) $ doDecode <$> [0 .. 0xFF]
 where
  doDecode x =
    (x, decode (x `shiftR` 6 .&. 0x03) (x `shiftR` 3 .&. 0x07) (x .&. 0x07))
  decode 0 0 0 = pure . Just $ NOP
  decode 0 1 0 = Just . LDI16I_SP <$> nextWord
  decode 0 2 0 = do
    b1 <- nextByte
    pure $ if b1 == 0 then Just STOP else Nothing
  decode 0 3 0 = Just . JR Nothing . fromIntegral <$> nextByte
  decode 0 cc 0 =
    Just . JR (conditionCode $ cc .&. 0x03) . fromIntegral <$> nextByte
  decode 0 dd 1 = if dd .&. 0x01 == 0
    then Just . LD16_I16 (registerPair dd) <$> nextWord
    else pure $ Just $ ADDHL (registerPair $ dd .&. 0x06)
  decode 0 0  2 = pure $ Just LDBCI_A
  decode 0 1  2 = pure $ Just LDA_BCI
  decode 0 2  2 = pure $ Just LDDEI_A
  decode 0 3  2 = pure $ Just LDA_DEI
  decode 0 4  2 = pure $ Just LDHLI_INC
  decode 0 5  2 = pure $ Just LDA_INC
  decode 0 6  2 = pure $ Just LDHLI_DEC
  decode 0 7  2 = pure $ Just LDA_DEC
  decode 0 ss 3 = pure . Just $ if ss .&. 0x01 == 0
    then INC16 $ registerPair ss
    else DEC16 . registerPair $ ss .&. 0x06
  decode 0 6  4  = pure . Just . INC $ SmallHLI
  decode 0 r  4  = pure . Just . INC $ SmallR8 (register r)
  decode 0 6  5  = pure . Just . DEC $ SmallHLI
  decode 0 r  5  = pure . Just . DEC $ SmallR8 (register r)
  decode 0 6  6  = Just . LDHLI_I8 <$> nextByte
  decode 0 r  6  = Just . LD_R8 (register r) . I8 <$> nextByte
  decode 0 0  7  = pure . Just $ RLCA
  decode 0 1  7  = pure . Just $ RRCA
  decode 0 2  7  = pure . Just $ RLA
  decode 0 3  7  = pure . Just $ RRA
  decode 0 4  7  = pure . Just $ DAA
  decode 0 5  7  = pure . Just $ CPL

  decode 1 6  6  = pure $ Just HALT
  decode 1 r  6  = pure $ Just $ LD_R8 (register r) HLI
  decode 1 6  r  = pure $ Just $ LDHLI_R8 (register r)
  decode 1 r1 r2 = pure $ Just $ LD_R8 (register r1) (R8 $register r2)

  decode 2 op 6  = pure . Just . aluOp op $ HLI
  decode 2 op r  = pure . Just . aluOp op . R8 $ register r

  decode 3 4  0  = Just . LDI8I_A <$> nextByte
  decode 3 5  0  = Just . ADDSP . fromIntegral <$> nextByte
  decode 3 6  0  = Just . LDA_I8I <$> nextByte
  decode 3 7  0  = Just . LDHL . fromIntegral <$> nextByte
  decode 3 cc 0  = pure . Just . RET $ conditionCode cc
  decode 3 1  1  = pure . Just $ RET Nothing
  decode 3 3  1  = pure . Just $ RETI
  decode 3 5  1  = pure . Just $ JPI
  decode 3 7  1  = pure . Just $ LDSP
  decode 3 qq 1  = pure . Just $ POP (registerPair qq)
  decode 3 4  2  = pure $ Just LDCI_A
  decode 3 5  2  = Just . LDI16I_A <$> nextWord
  decode 3 6  2  = pure $ Just LDA_CI
  decode 3 7  2  = Just . LDA_I16I <$> nextWord
  decode 3 cc 2  = Just . JP (conditionCode cc) <$> nextWord
  decode 3 0  3  = Just . JP Nothing <$> nextWord
  decode 3 cc 4  = Just . CALL (conditionCode cc) <$> nextWord
  decode 3 1  5  = Just . CALL Nothing <$> nextWord
  decode 3 qq 5  = pure . Just $ PUSH (registerPair qq)
  decode 3 op 6  = Just . aluOp op . I8 <$> nextByte
  decode 3 t  7  = pure . Just $ RST t

  decode 3 1  3  = do
    b1 <- nextByte
    pure $ case splitByte b1 of
      (0, 0, 6) -> Just $ RLC SmallHLI
      (0, 0, r) -> Just . RLC . SmallR8 $ register r
      (0, 1, 6) -> Just $ RRC SmallHLI
      (0, 1, r) -> Just . RRC . SmallR8 $ register r
      (0, 2, 6) -> Just $ RL SmallHLI
      (0, 2, r) -> Just . RL . SmallR8 $ register r
      (0, 3, 6) -> Just $ RR SmallHLI
      (0, 3, r) -> Just . RR . SmallR8 $ register r
      (0, 4, 6) -> Just $ SLA SmallHLI
      (0, 4, r) -> Just . SLA . SmallR8 $ register r
      (0, 5, 6) -> Just $ SRA SmallHLI
      (0, 5, r) -> Just . SRA . SmallR8 $ register r
      (0, 6, 6) -> Just $ SWAP SmallHLI
      (0, 6, r) -> Just . SWAP . SmallR8 $ register r
      (0, 7, 6) -> Just $ SRL SmallHLI
      (0, 7, r) -> Just . SRL . SmallR8 $ register r
      (1, b, 6) -> Just $ BIT b SmallHLI
      (1, b, r) -> Just . BIT b . SmallR8 $ register r
      (2, b, 6) -> Just $ RES b SmallHLI
      (2, b, r) -> Just . RES b . SmallR8 $ register r
      (3, b, 6) -> Just $ SET b SmallHLI
      (3, b, r) -> Just . SET b . SmallR8 $ register r
      _         -> Nothing

  decode _ _ _ = pure Nothing

  splitByte x = (x `shiftR` 6 .&. 0x03, x `shiftR` 3 .&. 0x07, x .&. 0x07)
  register 0o0 = RegB
  register 0o1 = RegC
  register 0o2 = RegD
  register 0o3 = RegE
  register 0o4 = RegH
  register 0o5 = RegL
  register 0o7 = RegA
  register r   = error $ "invalid register code " ++ show r
  registerPair 0 = RegBC
  registerPair 2 = RegDE
  registerPair 4 = RegHL
  registerPair 6 = RegSP
  registerPair x = error $ "invalid register pair code " ++ show x
  conditionCode 0 = Just CondNZ
  conditionCode 1 = Just CondZ
  conditionCode 2 = Just CondNC
  conditionCode 3 = Just CondC
  conditionCode x = error $ "invalid condition code " ++ show x
  aluOp 0 = ADD
  aluOp 1 = ADC
  aluOp 2 = SUB
  aluOp 3 = SBC
  aluOp 4 = AND
  aluOp 5 = XOR
  aluOp 6 = OR
  aluOp 7 = CP
  aluOp x = error $ "invalid ALU operation code " ++ show x

disassembleN :: Memory -> Word16 -> Int -> IO [(Word16, Instruction)]
disassembleN mem base len0 = evalStateT (runReaderT (doDisassemble len0) mem)
                                        base
 where
  doDisassemble :: Int -> Disassembler [(Word16, Instruction)]
  doDisassemble 0   = pure []
  doDisassemble len = do
    location <- get
    disassemble >>= \case
      Nothing -> pure []
      Just instruction ->
        ((location, instruction) :) <$> doDisassemble (len - 1)

disassemble :: Disassembler (Maybe Instruction)
disassemble = do
  b0 <- nextByte
  table ! b0

nextByte :: Disassembler Word8
nextByte = do
  mem  <- ask
  addr <- get
  r    <- liftIO $ readByte mem addr
  modify (+ 1)
  pure r

nextWord :: Disassembler Word16
nextWord = do
  mem  <- ask
  addr <- get
  l    <- liftIO $ fromIntegral <$> readByte mem addr
  h    <- liftIO $ fromIntegral <$> readByte mem (addr + 1)
  modify (+ 2)
  pure $ (h `unsafeShiftL` 8) .|. l

