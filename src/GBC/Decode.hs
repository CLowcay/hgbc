{-# LANGUAGE LambdaCase #-}

module GBC.Decode where

import           GBC.Memory
import           GBC.ISA
import           Data.Bits
import           Data.Array
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Word

type Decode a = ReaderT Memory (StateT Word16 IO) a

table :: Array Word8 (Decode (Maybe Instruction))
table = array (0, 0xFF) $ doDecode <$> [0 .. 0xFF]
 where
  doDecode x =
    ( x
    , decodeBytes (x `shiftR` 6 .&. 0x03) (x `shiftR` 3 .&. 0x07) (x .&. 0x07)
    )
  decodeBytes 0 0 0 = pure . Just $ NOP
  decodeBytes 0 1 0 = Just . LDI16I_SP <$> nextWord
  decodeBytes 0 2 0 = do
    b1 <- nextByte
    pure $ if b1 == 0 then Just STOP else Nothing
  decodeBytes 0 3 0 = Just . JR Nothing . fromIntegral <$> nextByte
  decodeBytes 0 cc 0 =
    Just . JR (conditionCode $ cc .&. 0x03) . fromIntegral <$> nextByte
  decodeBytes 0 dd 1 = if dd .&. 0x01 == 0
    then Just . LD16_I16 (registerPair dd) <$> nextWord
    else pure $ Just $ ADDHL (registerPair $ dd .&. 0x06)
  decodeBytes 0 0  2 = pure $ Just LDBCI_A
  decodeBytes 0 1  2 = pure $ Just LDA_BCI
  decodeBytes 0 2  2 = pure $ Just LDDEI_A
  decodeBytes 0 3  2 = pure $ Just LDA_DEI
  decodeBytes 0 4  2 = pure $ Just LDHLI_INC
  decodeBytes 0 5  2 = pure $ Just LDA_INC
  decodeBytes 0 6  2 = pure $ Just LDHLI_DEC
  decodeBytes 0 7  2 = pure $ Just LDA_DEC
  decodeBytes 0 ss 3 = pure . Just $ if ss .&. 0x01 == 0
    then INC16 $ registerPair ss
    else DEC16 . registerPair $ ss .&. 0x06
  decodeBytes 0 6  4  = pure . Just . INC $ SmallHLI
  decodeBytes 0 r  4  = pure . Just . INC $ SmallR8 (register r)
  decodeBytes 0 6  5  = pure . Just . DEC $ SmallHLI
  decodeBytes 0 r  5  = pure . Just . DEC $ SmallR8 (register r)
  decodeBytes 0 6  6  = Just . LDHLI_I8 <$> nextByte
  decodeBytes 0 r  6  = Just . LD_R8 (register r) . I8 <$> nextByte
  decodeBytes 0 0  7  = pure . Just $ RLCA
  decodeBytes 0 1  7  = pure . Just $ RRCA
  decodeBytes 0 2  7  = pure . Just $ RLA
  decodeBytes 0 3  7  = pure . Just $ RRA
  decodeBytes 0 4  7  = pure . Just $ DAA
  decodeBytes 0 5  7  = pure . Just $ CPL

  decodeBytes 1 6  6  = pure $ Just HALT
  decodeBytes 1 r  6  = pure $ Just $ LD_R8 (register r) HLI
  decodeBytes 1 6  r  = pure $ Just $ LDHLI_R8 (register r)
  decodeBytes 1 r1 r2 = pure $ Just $ LD_R8 (register r1) (R8 $register r2)

  decodeBytes 2 op 6  = pure . Just . aluOp op $ HLI
  decodeBytes 2 op r  = pure . Just . aluOp op . R8 $ register r

  decodeBytes 3 4  0  = Just . LDI8I_A <$> nextByte
  decodeBytes 3 5  0  = Just . ADDSP . fromIntegral <$> nextByte
  decodeBytes 3 6  0  = Just . LDA_I8I <$> nextByte
  decodeBytes 3 7  0  = Just . LDHL . fromIntegral <$> nextByte
  decodeBytes 3 cc 0  = pure . Just . RET $ conditionCode cc
  decodeBytes 3 1  1  = pure . Just $ RET Nothing
  decodeBytes 3 3  1  = pure . Just $ RETI
  decodeBytes 3 5  1  = pure . Just $ JPI
  decodeBytes 3 7  1  = pure . Just $ LDSP
  decodeBytes 3 qq 1  = pure . Just $ POP (registerPair qq)
  decodeBytes 3 4  2  = pure $ Just LDCI_A
  decodeBytes 3 5  2  = Just . LDI16I_A <$> nextWord
  decodeBytes 3 6  2  = pure $ Just LDA_CI
  decodeBytes 3 7  2  = Just . LDA_I16I <$> nextWord
  decodeBytes 3 cc 2  = Just . JP (conditionCode cc) <$> nextWord
  decodeBytes 3 0  3  = Just . JP Nothing <$> nextWord
  decodeBytes 3 cc 4  = Just . CALL (conditionCode cc) <$> nextWord
  decodeBytes 3 1  5  = Just . CALL Nothing <$> nextWord
  decodeBytes 3 qq 5  = pure . Just $ PUSH (registerPair qq)
  decodeBytes 3 op 6  = Just . aluOp op . I8 <$> nextByte
  decodeBytes 3 t  7  = pure . Just $ RST t

  decodeBytes 3 1  3  = do
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

  decodeBytes _ _ _ = pure Nothing

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

decodeN :: Memory -> Word16 -> Int -> IO [(Word16, Instruction)]
decodeN mem base len0 = evalStateT (runReaderT (doDecode len0) mem) base
 where
  doDecode :: Int -> Decode [(Word16, Instruction)]
  doDecode 0   = pure []
  doDecode len = do
    location <- get
    decode >>= \case
      Nothing          -> pure []
      Just instruction -> ((location, instruction) :) <$> doDecode (len - 1)

decode :: Decode (Maybe Instruction)
decode = do
  b0 <- nextByte
  table ! b0

nextByte :: Decode Word8
nextByte = do
  mem  <- ask
  addr <- get
  r    <- liftIO $ readByte mem addr
  modify (+ 1)
  pure r

nextWord :: Decode Word16
nextWord = do
  mem  <- ask
  addr <- get
  l    <- liftIO $ fromIntegral <$> readByte mem addr
  h    <- liftIO $ fromIntegral <$> readByte mem (addr + 1)
  modify (+ 2)
  pure $ (h `unsafeShiftL` 8) .|. l
