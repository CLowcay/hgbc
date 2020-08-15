{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Machine.GBC.CPU.Decode
  ( MonadFetch (..),
    decodeAndExecute,
    table0,
    table1,
  )
where

import Data.Bits
import qualified Data.Vector as V
import Data.Word
import Machine.GBC.CPU.ISA
import Machine.GBC.Util

class Monad m => MonadFetch m where
  nextByte :: m Word8

{-# INLINE nextWord #-}
nextWord :: MonadFetch m => m Word16
nextWord = do
  l <- nextByte
  h <- nextByte
  pure (fromIntegral l .|. (fromIntegral h .<<. 8))

{-# INLINE decodeAndExecute #-}
decodeAndExecute :: (MonadGMBZ80 m, MonadFetch m) => Word8 -> m (ExecuteResult m)
decodeAndExecute b0 = V.unsafeIndex table0 (fromIntegral b0)

{-# INLINEABLE table0 #-}
table0 :: (MonadGMBZ80 m, MonadFetch m) => V.Vector (m (ExecuteResult m))
table0 = V.generate 256 (decodeByte0 . splitByte . fromIntegral)

{-# INLINEABLE table1 #-}
table1 :: (MonadGMBZ80 m) => V.Vector (m (ExecuteResult m))
table1 = V.generate 256 (decodeByte1 . splitByte . fromIntegral)

decodeByte0 :: (MonadGMBZ80 m, MonadFetch m) => (Word8, Word8, Word8) -> m (ExecuteResult m)
decodeByte0 (0, 0, 0) = nop
decodeByte0 (0, 1, 0) = ldnnSP =<< nextWord
decodeByte0 (0, 2, 0) = do
  b1 <- nextByte
  if b1 == 0 then stop else invalid 0o020
decodeByte0 (0, 3, 0) = jr . fromIntegral =<< nextByte
decodeByte0 (0, xcc, 0) =
  let cc = conditionCode (xcc .&. 3) in jrcc cc . fromIntegral =<< nextByte
decodeByte0 (0, xss, 1) =
  let ss = registerSS (xss .&. 6) in if xss .&. 1 == 0 then ldddnn ss =<< nextWord else addhlss ss
decodeByte0 (0, 0, 2) = ldBCa
decodeByte0 (0, 1, 2) = ldaBC
decodeByte0 (0, 2, 2) = ldDEa
decodeByte0 (0, 3, 2) = ldaDE
decodeByte0 (0, 4, 2) = ldHLIa
decodeByte0 (0, 5, 2) = ldaHLI
decodeByte0 (0, 6, 2) = ldHLDa
decodeByte0 (0, 7, 2) = ldaHLD
decodeByte0 (0, xss, 3) =
  let ss = registerSS (xss .&. 6) in if xss .&. 1 == 0 then incss ss else decss ss
decodeByte0 (0, 6, 4) = inchl
decodeByte0 (0, r, 4) = let reg = register r in incr reg
decodeByte0 (0, 6, 5) = dechl
decodeByte0 (0, r, 5) = let reg = register r in decr reg
decodeByte0 (0, 6, 6) = ldHLn =<< nextByte
decodeByte0 (0, r, 6) = let reg = register r in ldrn reg =<< nextByte
decodeByte0 (0, 0, 7) = rlca
decodeByte0 (0, 1, 7) = rrca
decodeByte0 (0, 2, 7) = rla
decodeByte0 (0, 3, 7) = rra
decodeByte0 (0, 4, 7) = daa
decodeByte0 (0, 5, 7) = cpl
decodeByte0 (0, 6, 7) = scf
decodeByte0 (0, 7, 7) = ccf
decodeByte0 (1, 6, 6) = halt
decodeByte0 (1, r, 6) = let reg = register r in ldrHL reg
decodeByte0 (1, 6, r) = let reg = register r in ldHLr reg
decodeByte0 (1, r1, r2) =
  let reg1 = register r1
      reg2 = register r2
   in ldrr reg1 reg2
decodeByte0 (2, 0, 6) = addhl
decodeByte0 (2, 0, r) = let reg = register r in addr reg
decodeByte0 (2, 1, 6) = adchl
decodeByte0 (2, 1, r) = let reg = register r in adcr reg
decodeByte0 (2, 2, 6) = subhl
decodeByte0 (2, 2, r) = let reg = register r in subr reg
decodeByte0 (2, 3, 6) = sbchl
decodeByte0 (2, 3, r) = let reg = register r in sbcr reg
decodeByte0 (2, 4, 6) = andhl
decodeByte0 (2, 4, r) = let reg = register r in andr reg
decodeByte0 (2, 5, 6) = xorhl
decodeByte0 (2, 5, r) = let reg = register r in xorr reg
decodeByte0 (2, 6, 6) = orhl
decodeByte0 (2, 6, r) = let reg = register r in orr reg
decodeByte0 (2, 7, 6) = cphl
decodeByte0 (2, 7, r) = let reg = register r in cpr reg
decodeByte0 (3, 4, 0) = ldna =<< nextByte
decodeByte0 (3, 5, 0) = addSP . fromIntegral =<< nextByte
decodeByte0 (3, 6, 0) = ldan =<< nextByte
decodeByte0 (3, 7, 0) = ldhl . fromIntegral =<< nextByte
decodeByte0 (3, xcc, 0) = let cc = conditionCode xcc in retcc cc
decodeByte0 (3, 1, 1) = ret
decodeByte0 (3, 3, 1) = reti
decodeByte0 (3, 5, 1) = jphl
decodeByte0 (3, 7, 1) = ldSPHL
decodeByte0 (3, qqx, 1) = let qq = registerQQ qqx in pop qq
decodeByte0 (3, 4, 2) = ldCa
decodeByte0 (3, 5, 2) = ldnna =<< nextWord
decodeByte0 (3, 6, 2) = ldaC
decodeByte0 (3, 7, 2) = ldann =<< nextWord
decodeByte0 (3, xcc, 2) = let cc = conditionCode xcc in jpccnn cc =<< nextWord
decodeByte0 (3, 0, 3) = jpnn =<< nextWord
decodeByte0 (3, 6, 3) = di
decodeByte0 (3, 7, 3) = ei
decodeByte0 (3, xcc, 4) =
  if xcc .&. 0x4 /= 0
    then invalid (0o304 .|. xcc .<<. 3)
    else let cc = conditionCode xcc in callcc cc =<< nextWord
decodeByte0 (3, 1, 5) = call =<< nextWord
decodeByte0 (3, 3, 5) = invalid 0o335
decodeByte0 (3, 5, 5) = invalid 0o355
decodeByte0 (3, 7, 5) = invalid 0o375
decodeByte0 (3, qqx, 5) = let qq = registerQQ qqx in push qq
decodeByte0 (3, 0, 6) = addn =<< nextByte
decodeByte0 (3, 1, 6) = adcn =<< nextByte
decodeByte0 (3, 2, 6) = subn =<< nextByte
decodeByte0 (3, 3, 6) = sbcn =<< nextByte
decodeByte0 (3, 4, 6) = andn =<< nextByte
decodeByte0 (3, 5, 6) = xorn =<< nextByte
decodeByte0 (3, 6, 6) = orn =<< nextByte
decodeByte0 (3, 7, 6) = cpn =<< nextByte
decodeByte0 (3, t, 7) = rst t
decodeByte0 (3, 1, 3) = do
  b1 <- nextByte
  V.unsafeIndex table1 (fromIntegral b1)
decodeByte0 (a, b, c) = invalid ((a .<<. 6) .|. (b .<<. 3) .|. c)

decodeByte1 :: MonadGMBZ80 m => (Word8, Word8, Word8) -> m (ExecuteResult m)
decodeByte1 (0, 0, 6) = rlchl
decodeByte1 (0, 0, r) = let reg = register r in rlcr reg
decodeByte1 (0, 1, 6) = rrchl
decodeByte1 (0, 1, r) = let reg = register r in rrcr reg
decodeByte1 (0, 2, 6) = rlhl
decodeByte1 (0, 2, r) = let reg = register r in rlr reg
decodeByte1 (0, 3, 6) = rrhl
decodeByte1 (0, 3, r) = let reg = register r in rrr reg
decodeByte1 (0, 4, 6) = slahl
decodeByte1 (0, 4, r) = let reg = register r in slar reg
decodeByte1 (0, 5, 6) = srahl
decodeByte1 (0, 5, r) = let reg = register r in srar reg
decodeByte1 (0, 6, 6) = swaphl
decodeByte1 (0, 6, r) = let reg = register r in swapr reg
decodeByte1 (0, 7, 6) = srlhl
decodeByte1 (0, 7, r) = let reg = register r in srlr reg
decodeByte1 (1, b, 6) = bithl b
decodeByte1 (1, b, r) = let reg = register r in bitr reg b
decodeByte1 (2, b, 6) = reshl b
decodeByte1 (2, b, r) = let reg = register r in resr reg b
decodeByte1 (3, b, 6) = sethl b
decodeByte1 (3, b, r) = let reg = register r in setr reg b
decodeByte1 (a, b, c) = invalid ((a .<<. 6) .|. (b .<<. 3) .|. c)

splitByte :: Word8 -> (Word8, Word8, Word8)
splitByte x = ((x .>>. 6) .&. 0x03, (x .>>. 3) .&. 0x07, x .&. 0x07)

register :: Word8 -> Register8
register 0o0 = RegB
register 0o1 = RegC
register 0o2 = RegD
register 0o3 = RegE
register 0o4 = RegH
register 0o5 = RegL
register 0o7 = RegA
register r = error ("invalid register code " <> show r)

registerQQ :: Word8 -> RegisterPushPop
registerQQ 0 = PushPopBC
registerQQ 2 = PushPopDE
registerQQ 4 = PushPopHL
registerQQ 6 = PushPopAF
registerQQ x = error ("invalid register pair code " <> show x)

registerSS :: Word8 -> Register16
registerSS 0 = RegBC
registerSS 2 = RegDE
registerSS 4 = RegHL
registerSS 6 = RegSP
registerSS x = error ("invalid register pair code " <> show x)

conditionCode :: Word8 -> ConditionCode
conditionCode 0 = CondNZ
conditionCode 1 = CondZ
conditionCode 2 = CondNC
conditionCode 3 = CondC
conditionCode x = error ("invalid condition code " <> show x)
